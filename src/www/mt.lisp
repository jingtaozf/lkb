;;; -*- Mode: Common-Lisp; Package: www; Encoding: utf-8; -*-

(in-package :lkb)

(defpackage :www
  (:use :net.aserve :net.html.generator :lkb :make :common-lisp))

(in-package :www)

(defparameter *www-port* 8008)

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

(defparameter *www-alttxt-js*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "alttxt.js"))

(defparameter *www-introduction*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "logon.html"))

(defparameter *www-icon*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "logon.gif"))

(defparameter *www-1x20*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "1x20.jpg"))

(defparameter *www-rmrs-dtd*
  (make-pathname
   :directory (pathname-directory
               (dir-append (get-sources-dir "www") '(:relative "rmrs")))
   :name "rmrs.dtd"))

(defparameter *www-maximal-number-of-edges* 20000)

(defparameter *www-maximal-number-of-results* 5)

(defvar %www-clients% 0)

(defvar %www-item-id% 0)

(defvar %www-object-counter% 0)

(defvar %www-attic% (make-array 512))

(defparameter *logon-authorizer*
  (make-instance 'password-authorizer
    :allowed '(("oe" . "1koelsch")
               ("logon" . "dal"))
    :realm "LOGON On-Line Demonstrator (Version 0.8)"))

(defun initialize (&key (port *www-port*) pattern)
  (let ((interrupt (format 
                    nil 
                    "/tmp/.aserve.~a.~a" 
                    (tsdb::current-user) port)))
    (unless (mp:process-p *www-interrupt*)
      (flet ((check-interrupt () 
               (loop
                 (when (probe-file interrupt)
                   (format
                    t
                    "check-interrupt(): exiting for `~a'~%"
                    interrupt)
                   (force-output t)
                   (delete-file interrupt)
                   (excl:exit))
                 (sleep 5))))
        (setf *www-interrupt*
          (mp:process-run-function 
              '(:name "aserve interrupt handler") #'check-interrupt))))
    (sleep 2)
    (setf %www-clients% 0)
    (setf %www-item-id% 0)
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
      (format
       t
       "initialize(): unable to bind port ~d; attempting interrupt.~%"
       port)
      (force-output t)
      (with-open-file (foo interrupt :direction :output :if-exists :supersede))
      (sleep 10)
      (start :port port :external-format (excl:crlf-base-ef :utf-8))))
    
  (publish-file :path "/lkb.css" :file *www-lkb-css*)
  (publish-file :path "/lkb.js" :file *www-lkb-js*)
  (publish-file :path "/alttxt.js" :file *www-alttxt-js*)
  (publish-file :path "/icon.gif" :file *www-icon*)
  (publish-file :path "/1x20.jpg" :file *www-1x20*)
  (publish-file :path "/rmrs.dtd" :file *www-rmrs-dtd*)

  (publish :path "/compare"
   :content-type "text/html"
   :function #'(lambda (request entity) (www-compare request entity)))

  (cond
   ((null pattern)
    (publish :path "/logon"
     :content-type "text/html"
     :authorizer *logon-authorizer*
     :function #'(lambda (request entity) (www-logon request entity)))
    (publish :path "/browse"
      :content-type "text/html"
      :function #'(lambda (request entity) (www-browse request entity)))
    (publish :path "/view"
      :content-type "text/html"
      :function #'(lambda (request entity) (www-view request entity))))
   (t
    (publish :path "/podium"
      :content-type "text/html"
      :function #'(lambda (request entity) 
                    (www-podium request entity :pattern pattern)))
    (publish :path "/itsdb"
      :content-type "text/html"
      :function #'(lambda (request entity) (www-itsdb request entity))))))

(defun www-logon (request entity)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (task (lookup-form-value "task" query))
         (input (or (lookup-form-value "input" query)
                    "Bergensområdet er tett befolket fremdeles."))
         (exhaustivep 
          (let ((foo (lookup-form-value "exhaustivep" query)))
            (or (null foo) (string-equal foo "all"))))
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
           ((equal nresults "5") 5)
           ((equal nresults "10") 10)
           ((equal nresults "50") 50)
           ((equal nresults "100") 100)
           ((equal nresults "500") 500)
           ((equal nresults "all") nil)
           (t *www-maximal-number-of-results*))))
    (with-http-response (request entity)
      (with-http-body (request entity
                               :external-format (excl:crlf-base-ef :utf-8))
        
        (www-doctype *html-stream*)
        (html (:html
               (www-header 
                *html-stream* 
                (cond
                 ((string-equal task "analyze")
                  "LOGON On-Line (Analysis)")
                 ((string-equal task "translate")
                  "LOGON On-Line (Translation)")
                 (t
                  "LOGON On-Line Demonstrator")))
               ((:body :onload "messenger()")
                (:center
                 (unless (eq method :post)
                   (www-output *www-introduction* :stream *html-stream*))
                 ((:form 
                   :action "/logon" :method "post" :id "main"
                   :onsubmit "submitter('main')"
                   :accept-charset "utf-8" :target "_self")
                  :newline
                  ((:input :type "reset" :value "Reset"))
                  "&nbsp;"
                  ((:input
                    :type "text" :name "input"
                    :value (or input "") :size "80"))
                  "&nbsp;"
                  ((:input 
                    :type "submit" :name "task" :id "analyze"
                    :value "Analyze" :disabled '||'
                    :onclick "setTarget('main', '_self')"))
                  "&nbsp;"
                  ((:input 
                    :type "submit" :name "task" :id "translate"
                    :value "Translate" :disabled '||'
                    :onclick "setTarget('main', '_self')"))
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
                      ((:option :value "5" :selected '||) "5")
                      ((:option :value "10") "10")
                      ((:option :value "50") "50")
                      ((:option :value "100") "100")
                      ((:option :value "500") "500")
                      ((:option :value "all") "all")))
                    ((:td :class "buttons") "&nbsp;results")))))
                (cond
                 ((and (string-equal task "analyze") input)
                  (www-parse
                   input
                   :exhaustivep exhaustivep :treep treep :mrsp mrsp
                   :request request :stream *html-stream*))
                 ((and (string-equal task "translate") input)
                  (www-translate
                   input :exhaustivep exhaustivep 
                   :request request :stream *html-stream*))
                 (t
                  (www-version *html-stream*))))))))))

(defvar %item% nil)

(defun www-parse (input &key exhaustivep treep mrsp request stream)

  (let* ((item (pairlis '(:i-id :parse-id :i-input 
                          :edges)
                        (list (incf %www-item-id%) 0 input
                              *www-maximal-number-of-edges*)))
         (nanalyses (if exhaustivep 0 1))
         (nresults (or *www-maximal-number-of-results* 0))
         (item 
          (setf %item%
            (tsdb::pvm-process 
             item :parse 
             :exhaustive exhaustivep
             :nanalyses nanalyses :nresults nresults)))
         (readings (tsdb::get-field :readings item))
         (nresults (or *www-maximal-number-of-results* readings))
         (time (tsdb::get-field :tcpu item))
         (time (and (numberp time) (/ time 1000)))
         (pedges (tsdb::get-field :pedges item))
         (results (tsdb::get-field :results item))
         (unique (length results))
         (error (tsdb::get-field :error item))
         (error (unless (and (numberp readings) (> readings 0))
                  (or
                   (loop
                       with end = 0 
                       with start with starts with ends
                       with result
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
                         with end = 0 with start = end 
                         with starts with ends
                         with result
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
    (format stream "<center>~%")
    (cond
     ((null error)
      (format
       stream 
       "<div id=summary>~
        [~d of ~d~@[ (of ~a)~] ~:[analyses~;analysis~]~
        ~@[; processing time: ~,2f seconds~]~
        ~@[; ~a edges~]]</div>~%~
        <br>~%"
       (if (numberp *www-maximal-number-of-results*)
         (min unique readings *www-maximal-number-of-results*)
         (min unique readings))
       (min unique readings)
       (and (not (= readings unique)) readings)
       (= readings 1)
       time pedges pedges)
      (loop
          with tsdb::*reconstruct-cache* = (make-hash-table :test #'eql)
          with mrs::*mrs-relations-per-row* = 5
          initially
            (format 
             stream 
             "<form action=\"/browse\" method=post id=\"main\" ~
                    accept-charset=\"utf-8\" target=\"_blank\">~%~
              <input type=hidden name=item value=~a>~%  ~
              <input type=hidden name=results value=~a>~%  ~
              <div id=action>~%  ~
              <input type=submit name=action value=compare>~%  ~
              <select name=set size=1>~%    ~
              <option value=all>all analyses</option>~%    ~
              <option value=active selected>selection</option>~%    ~
              </select>~%  ~
              &nbsp;&nbsp;|&nbsp;&nbsp;~%  ~
              &nbsp;&nbsp;|&nbsp;&nbsp;~%  ~
              <input type=submit name=action value=transfer ~
                     onclick=\"setTarget('main', 'transfer')\">~%  ~
              <input type=submit name=action value=generate ~
                     onclick=\"setTarget('main', 'generate')\">~%  ~
              <input type=submit name=action value=avm disabled>&nbsp;~%  ~
              <input type=submit name=action value=scope disabled>&nbsp;~%  ~
              </div>~%<table class=results>~%"
             (www-store-object nil item) (www-store-object nil results))
          finally (format stream "</table></form>~%")
          for i from 0
          for result in results
          for derivation = (tsdb::get-field :derivation result)
          for mrs = (mrs::read-mrs-from-string (tsdb::get-field :mrs result))
          for edge = (when (or treep (and mrsp (null mrs)))
                       (or (tsdb::get-field :edge result)
                           (when derivation 
                             (let ((edge (tsdb::reconstruct derivation)))
                               (setf (lkb::edge-mrs edge) mrs)
                               edge))))
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
             i i)
          when (and treep edge) do
            (format stream "<td class=resultsTree>~%")
            (lkb::html-tree edge :stream stream :indentation 4)
            (format stream "</td>~%")
          when (and mrsp (or mrs edge)) do
            (format stream "<td class=resultsMrs>~%")
            (mrs::output-mrs1 
             (or mrs (mrs::extract-mrs edge))'mrs::html stream i)
            (format stream "</td>~%")
          do (format stream "</tr>")))
     ((or (null error) (equal error ""))
      (format
       stream 
       "<div id=error>~
        <p>No result(s) were found for this input.&nbsp;&nbsp;~
        Is it well-formed?</p>~%~
        </div>~%"))
     ((integerp error)
      (format
       stream 
       "<div id=error>~
        <p>The parser exhausted its search space limit ~
        (of ~d passive edge~p);<br>~
        try non-exhaustive parsing or a shorter (or less ambiguous) ~
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
    (format stream "</center>~%")
    (www-version stream)))

(defun www-translate (input &key exhaustivep request stream)

  (format stream "<center>~%")
  (let* ((n (if exhaustivep *www-maximal-number-of-results* 1))
         (result (setf %item%
                   (tsdb::translate-string 
                    input :stream stream :format :html :nhypotheses n
                    :index %www-object-counter%))))
    ;;
    ;; at this point, we rely on translate-string() to have arranged for items
    ;; to be rendered with anchors using object ids in the order corresponding
    ;; to those assigned by the www-store-object() calls below.  this mainly
    ;; serves to increase modularity, i.e. spare translate-string() from having
    ;; to do the actual object storage.
    ;;
    (let* ((www (tsdb::get-field :www result))
           (id (and (numberp www)(www-store-object nil result))))
      (unless (= www id)
        (www-warn 
         request
         (format 
          nil
          "www-translate(): object id mismatch (~a != ~a)"
          www id))))
    (loop
        for transfer in (tsdb::get-field :transfers result)
        for www = (tsdb::get-field :www transfer)
        for id = (and (numberp www)(www-store-object nil transfer))
        for realizations = (tsdb::get-field :realizations transfer)
        unless (= www id) do
          (www-warn 
           request
           (format 
            nil
            "www-translate(): object id mismatch (~a != ~a)"
            www id))
        do
          (loop
              for realization in realizations
              for www = (tsdb::get-field :www realization)
              for id = (and (numberp www) (www-store-object nil realization))
              unless (= www id) do
                (www-warn 
                 request
                 (format 
                  nil
                  "www-translate(): object id mismatch (~a != ~a)"
                  www id))))
    
    (when request 
      #+:null
      (www-log request input readings time pedges error))
    (format stream "</center>~%")
    (www-version stream)))

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
     ((string-equal action "compare")
      (when (and selection (string-equal set "active"))
        (loop
            with all = (www-retrieve-object nil results)
            with active = nil
            for foo in (if (listp selection) selection (list selection))
            for i = (ignore-errors (parse-integer foo))
            for edge = (and i (nth i all))
            when results do (push edge active)
            finally (setf results (www-store-object nil active))))
      (www-compare request entity :results results))
     ((string-equal action "transfer")
      (when (and selection (string-equal set "active"))
        (loop
            with all = (www-retrieve-object nil results)
            with active = nil
            for foo in (if (listp selection) selection (list selection))
            for i = (ignore-errors (parse-integer foo))
            for edge = (and i (nth i all))
            when results do (push edge active)
            finally (setf results (www-store-object nil active))))
      (www-process request entity :results results :type :transfer))
     ((string-equal action "generate")
      (when (and selection (string-equal set "active"))
        ;;
        ;; for generation, we can really only take in one result at a time
        ;;
        (loop
            with all = (www-retrieve-object nil results)
            for foo in (if (listp selection) selection (list selection))
            for i = (ignore-errors (parse-integer foo))
            for edge = (and i (nth i all))
            when edge do 
              (setf results (www-store-object nil (list edge)))
              (return)))
      (www-process request entity :results results :type :generate)))))

(defun www-process (request entity &key type results (wait 5))
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (item (if query 
                 (lookup-form-value "item" query)
                 (request-query-value "item" request :post nil)))
         (item (typecase item
                 (string (ignore-errors (parse-integer item)))
                 (number item)))
         (item (www-retrieve-object nil item))
         (results (or results
                      (if query 
                        (lookup-form-value "results" query)
                        (request-query-value "results" request :post nil))))
         (results (typecase results
                    (string (ignore-errors (parse-integer results)))
                    (number results)))
         (results (www-retrieve-object nil results))
         (results (stable-sort 
                   results #'< 
                   :key #'(lambda (foo) (tsdb::get-field :result-id foo))))
         (item (acons 
                :ranks
                (loop
                    for i from 1
                    for result in results
                    collect (acons :rank i result))
                item))
         (nresults (lookup-form-value "nresults" query))
         (nresults
          (cond
           ((equal nresults "5") 5)
           ((equal nresults "10") 10)
           ((equal nresults "50") 50)
           ((equal nresults "100") 100)
           ((equal nresults "500") 500)
           ((equal nresults "all") nil)
           (t *www-maximal-number-of-results*)))
         (item (setf %item% (tsdb::pvm-process item type :wait wait)))
         (readings (tsdb::get-field :readings item))
         (time (tsdb::get-field :tcpu item))
         (time (and (numberp time) (/ time 1000)))
         (pedges (tsdb::get-field :pedges item))
         (results (tsdb::get-field :results item))
         (rawp nil)
         (error (tsdb::get-field :error item))
         (error (unless (and (numberp readings) (> readings 0))
                  (or
                   (loop
                       with end = 0 
                       with start with starts with ends
                       with result
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
                         with end = 0 with start = end 
                         with starts with ends
                         with result
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
                   (when (or (search "invalid predicates" error)
                             (search "unknown input relation" error))
                     (setf rawp t)
                     error)
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
    (when request 
      (www-log 
       request (tsdb::get-field :i-input item) readings time pedges error))

    (with-http-response (request entity)
      (with-http-body (request entity
                       :external-format (excl:crlf-base-ef :utf-8))
        
        (www-doctype *html-stream*)
        (html (:html
               (www-header 
                *html-stream* 
                (case type
                  (:transfer "LOGON On-Line (Transfer)")
                  (:generate "LOGON On-Line (Generation)"))
                (case type
                  (:transfer "transfer")
                  (:generate "generate")))
               ((:body :onload "messenger()")
                (:center
                 (unless (eq method :post)
                   (www-output *www-introduction* :stream *html-stream*))
                 ((:form 
                   :action "/browse" :method "post" 
                   :id "main" :target "_blank"
                   :accept-charset "utf-8")
                  :newline
                  (:center
                   (cond
                    ((null error)
                     (format
                      *html-stream* 
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
                        with tsdb::*reconstruct-cache* 
                        = (make-hash-table :test #'eql)
                        with mrs::*mrs-relations-per-row* = 5
                        initially
                          (format 
                           *html-stream* 
                           "<input type=hidden name=item value=~a>~%  ~
                            <input type=hidden name=results value=~a>~%  ~
                            <div id=action>~%  ~
                            <input type=button name=close value=close ~
                                   onClick='window.close()'>&nbsp;&nbsp;~
                            <input type=submit name=action value=compare ~
                                   disabled>~%  ~
                            <select name=set size=1>~%    ~
                              <option value=all>all analyses</option>~%    ~
                              <option value=active ~
                                      selected>selection</option>~%    ~
                            </select>~%  ~
                            &nbsp;&nbsp;|&nbsp;&nbsp;~%  ~
                            <input type=submit name=action ~
                                   onclick=\"setTarget('main', 'transfer')\" ~
                                   value=transfer>~%   ~
                            <input type=submit name=action ~
                                   onclick=\"setTarget('main', 'generate')\" ~
                                   value=generate>~%  ~
                            ~@[~*<input type=submit name=action value=avm ~
                                        disabled>&nbsp;~%  ~]~
                            <input type=submit name=action value=scope ~
                                   disabled>&nbsp;~%  ~
                            &nbsp;&nbsp;|&nbsp;&nbsp;show&nbsp;~%~
                            <select size=1 name=nresults>~
                              <option value=\"5\" selected>10</option>~
                              <option value=\"10\">10</option>~
                              <option value=\"50\">50</option>~
                              <option value=\"100\">100</option>~
                              <option value=\"all\">all</option>~
                            </select>~%  &nbsp;results~%  ~
                            </div>~%"
                           (www-store-object nil item) 
                           (www-store-object nil results)
                           (not (eq type :transfer)))
                          (when (and (eq type :generate) (> readings 0))
                            (format 
                             *html-stream*
                             "<div class=generatorOutputs>~
                              <table class=generatorOutputs>~%")
                            (loop
                                for i from 0
                                for result in results
                                for tree = (tsdb::get-field :tree result)
                                for class = (determine-string-class tree)
                                for score = (tsdb::get-field :score result)
                                when (stringp tree) do
                                  (format
                                   *html-stream*
                                   "<tr>~
                                      <td class=generatorOutputIndex>~
                                        (~a)&nbsp;&nbsp;</td>~
                                      <td class=~
                                            generator~:[~*~;~:(~a~)~]Output>~
                                        <a href=\"#result~a\">~a</a></td>~
                                      <td class=generatorOutputScore>~
                                        ~@[&nbsp;&nbsp;[~,1f]~]</td>~
                                    </tr>~%"
                                   i class class i tree score))
                            (format *html-stream* "</table></div>~%"))
                          (format *html-stream* "<table class=results>~%")
                        finally (format *html-stream* "</table></form>~%")
                        for i from 0
                        for result in results
                        for derivation = (tsdb::get-field :derivation result)
                        for mrs = (mrs::read-mrs-from-string 
                                   (tsdb::get-field :mrs result))
                        for edge = (or (tsdb::get-field :edge result)
                                       (and derivation 
                                            (tsdb::reconstruct derivation)))
                        for tree = (tsdb::get-field :tree result)
                        while (< i nresults) 
                        do (when edge (nconc result (acons :edge edge nil)))
                        when (or mrs edge (and tree (eq type :transfer))) do
                          (format 
                           *html-stream* 
                           "<tr>~%<td class=resultsNavigation>~%  ~
                            <table class=resultsNavigation>~%    ~
                            <a name=\"result~a\"><tr><td class=center>~%~
                            <div class=resultsNavigation># ~a</div></td>~
                            </tr>~%    ~
                            <tr><td class=center>~
                            <input type=checkbox name=selection value=\"~a\">~
                            </td></tr>~%  ~
                            </table></td>~%"
                           i i i)
                        when (and edge (not (eq type :transfer))) do
                          (format *html-stream* "<td class=resultsTree>~%")
                          (lkb::html-tree 
                           edge :stream *html-stream* :indentation 4)
                          (format *html-stream* "</td>~%")
                        when (and tree (eq type :transfer)) do
                          (format 
                           *html-stream*
                           "<td class=resultsDerivations>~%")
                          (format 
                           *html-stream* 
                           "<table class=resultsDerivations>~%")
                          #+:mt
                          (loop
                              for derivation
                              = (mt::read-derivation-from-string tree)
                              then (mt::edge-daughter derivation)
                              while (and (mt::edge-p derivation) 
                                         (mt::edge-daughter derivation))
                              do
                                (format
                                 *html-stream*
                                 "<td class=resultsDerivation>~
                                    ~(~a~)&nbsp;&nbsp;[~a]</td></tr>~%"
                                 (mt::edge-rule derivation)
                                 (mt::edge-id derivation)))
                          (format *html-stream* "</table></td>~%")
                        when (or mrs edge) do
                          (format *html-stream* "<td class=resultsMrs>~%")
                          (mrs::output-mrs1 
                           (or mrs (mrs::extract-mrs edge))
                           'mrs::html *html-stream* i)
                          (format *html-stream* "</td>~%")
                        do (format *html-stream* "</tr>")))
                   ((or (null error) (equal error ""))
                    (format
                     *html-stream* 
                     "<div id=error>~
                      <p>No result(s) were found for this input.&nbsp;&nbsp;~
                      Is it grammatical?</p>~%~
                      </div>~%"))
                   ((integerp error)
                    (format
                     *html-stream* 
                     "<div id=error>~
                      <p>The processor exhausted its search space limit ~
                      (of ~d passive edge~p);<br>~
                      try non-exhaustive processing or a shorter ~
                      (or less ambiguous) ~
                      input.</p>~%</div>~%"
                     error error))
                   ((consp error)
                    (format
                     *html-stream* 
                     "<div id=error>~
                      The following input tokens were ~
                      not recognized by the processor: ~
                      ~{&lsquo;~(~a~)&rsquo;~^ ~}.~%</div>~%"
                     error))
                   ((and rawp (stringp error))
                    (format
                     *html-stream* 
                     "<div id=error>~a.~%</div>~%"
                     (string-right-trim '(#\. #\? #\!) error)))
                   (t
                    (format
                     *html-stream* 
                     "<div id=error>~
                      The server encountered an (unexpected) error: ~
                      &lsquo;~a&rsquo;.~%</div>~%"
                     (string-right-trim '(#\. #\? #\!) error))))
                  (www-version *html-stream*)))))))))))

(defun www-view (request entity &key type item nresults)
  #+:debug
  (setf %request% request %entity% entity)
  (let* ((method (request-method request))
         (body (when (eq method :post) (get-request-body request)))
         (query (and body (form-urlencoded-to-query body)))
         (item 
          (or item
              (let* ((item (if query 
                             (lookup-form-value "item" query)
                             (request-query-value "item" request :post nil)))
                     (item (typecase item
                             (string (ignore-errors (parse-integer item)))
                             (number item))))
                (www-retrieve-object nil item))))
         (nresults (or nresults (lookup-form-value "nresults" query)))
         (nresults
          (cond
           ((equal nresults "5") 5)
           ((equal nresults "10") 10)
           ((equal nresults "50") 50)
           ((equal nresults "100") 100)
           ((equal nresults "500") 500)
           ((equal nresults "all") nil)
           (t *www-maximal-number-of-results*)))
         (type (or type
                   (cond
                    ((null item) :unknown)
                    ((tsdb::get-field :transfers item) :parse)
                    ((tsdb::get-field :realizations item) :transfer)
                    (t :generate))))
         (readings (tsdb::get-field :readings item))
         (time (tsdb::get-field :tcpu item))
         (time (and (numberp time) (/ time 1000)))
         (pedges (tsdb::get-field :pedges item))
         (results (tsdb::get-field :results item))
         (rawp nil)
         (error (tsdb::get-field :error item))
         (error (unless (and (numberp readings) (> readings 0))
                  (or
                   (loop
                       with end = 0 
                       with start with starts with ends
                       with result
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
                         with end = 0 with start = end 
                         with starts with ends
                         with result
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
                   (when (or (search "invalid predicates" error)
                             (search "unknown input relation" error))
                     (setf rawp t)
                     error)
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
    (when request 
      (www-log 
       request (tsdb::get-field :i-input item) readings time pedges error))

    (with-http-response (request entity)
      (with-http-body (request entity
                       :external-format (excl:crlf-base-ef :utf-8))
        
        (www-doctype *html-stream*)
        (html (:html
               (www-header 
                *html-stream* 
                (case type
                  (:parse "LOGON On-Line (Analysis)")
                  (:transfer "LOGON On-Line (Transfer)")
                  (:generate "LOGON On-Line (Generation)")
                  (t "LOGON On-Line"))
                ;;
                ;; in case we were called as a call-back from the fan-out HTML,
                ;; then all viewing targets a new window.
                ;;
                (if (null query)
                  (gensym "")
                  (case type
                    (:parse "parse")
                    (:transfer "transfer")
                    (:generate "generate")
                    (t (gensym "")))))
               ((:body :onload "messenger()")
                (:center
                 ((:form 
                   :action "/browse" :method "post" 
                   :id "main" :target "_blank"
                   :onsubmit "submitter('main')"
                   :accept-charset "utf-8")
                  :newline
                  (:center
                   (cond
                    ((null error)
                     (format
                      *html-stream* 
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
                        with tsdb::*reconstruct-cache* 
                        = (make-hash-table :test #'eql)
                        with mrs::*mrs-relations-per-row* = 5
                        initially
                          (format 
                           *html-stream* 
                           "<input type=hidden name=item value=~a>~%  ~
                            <input type=hidden name=results value=~a>~%  ~
                            <div id=action>~%  ~
                            <input type=button name=close value=close ~
                                   onClick='window.close()'>&nbsp;&nbsp;~
                            <input type=submit name=action value=compare ~
                                   disabled>~%  ~
                            <select name=set size=1>~%    ~
                              <option value=all>all analyses</option>~%    ~
                              <option value=active ~
                                      selected>selection</option>~%    ~
                            </select>~%  ~
                            &nbsp;&nbsp;|&nbsp;&nbsp;~%  ~
                            <input type=submit name=action ~
                                   onclick=\"setTarget('main', 'transfer')\" ~
                                   value=transfer>~%   ~
                            <input type=submit name=action ~
                                   onclick=\"setTarget('main', 'generate')\" ~
                                   value=generate>~%  ~
                            ~@[~*<input type=submit name=action value=avm ~
                                        disabled>&nbsp;~%  ~]~
                            <input type=submit name=action value=scope ~
                                   disabled>&nbsp;~%  ~
                            &nbsp;&nbsp;|&nbsp;&nbsp;show&nbsp;~%~
                            <select size=1 name=nresults>~
                              <option value=\"5\" selected>10</option>~
                              <option value=\"10\">10</option>~
                              <option value=\"50\">50</option>~
                              <option value=\"100\">100</option>~
                              <option value=\"all\">all</option>~
                            </select>~%  &nbsp;results~%  ~
                            </div>~%"
                           (www-store-object nil item) 
                           (www-store-object nil results)
                           (not (eq type :transfer)))
                          (when (and (eq type :generate) (> readings 0))
                            (format 
                             *html-stream*
                             "<div class=generatorOutputs>~
                              <table class=generatorOutputs>~%")
                            (loop
                                for i from 0
                                for result in results
                                for tree = (tsdb::get-field :tree result)
                                for class = (determine-string-class tree)
                                for score = (tsdb::get-field :score result)
                                when (stringp tree) do
                                  (format
                                   *html-stream*
                                   "<tr>~
                                      <td class=generatorOutputIndex>~
                                        (~a)&nbsp;&nbsp;</td>~
                                      <td class=~
                                            generator~:[~*~;~:(~a~)~]Output>~
                                        <a href=\"#result~a\">~a</a></td>~
                                      <td class=generatorOutputScore>~
                                        ~@[&nbsp;&nbsp;[~,1f]~]</td>~
                                    </tr>~%"
                                   i class class i tree score))
                            (format *html-stream* "</table></div>~%"))
                          (format *html-stream* "<table class=results>~%")
                        finally (format *html-stream* "</table></form>~%")
                        for i from 0
                        for result in results
                        for derivation = (tsdb::get-field :derivation result)
                        for mrs = (mrs::read-mrs-from-string 
                                   (tsdb::get-field :mrs result))
                        for edge = (or (tsdb::get-field :edge result)
                                       (and derivation 
                                            (tsdb::reconstruct derivation)))
                        for tree = (tsdb::get-field :tree result)
                        while (< i nresults) 
                        do (when edge (nconc result (acons :edge edge nil)))
                        when (or mrs edge (and tree (eq type :transfer))) do
                          (format 
                           *html-stream* 
                           "<tr>~%<td class=resultsNavigation>~%  ~
                            <table class=resultsNavigation>~%    ~
                            <a name=\"result~a\"><tr><td class=center>~%~
                            <div class=resultsNavigation># ~a</div></td>~
                            </tr>~%    ~
                            <tr><td class=center>~
                            <input type=checkbox name=selection value=\"~a\">~
                            </td></tr>~%  ~
                            </table></td>~%"
                           i i i)
                        when (and edge (not (eq type :transfer))) do
                          (format *html-stream* "<td class=resultsTree>~%")
                          (lkb::html-tree 
                           edge :stream *html-stream* :indentation 4)
                          (format *html-stream* "</td>~%")
                        when (and tree (eq type :transfer)) do
                          (format 
                           *html-stream*
                           "<td class=resultsDerivations>~%")
                          (format 
                           *html-stream* 
                           "<table class=resultsDerivations>~%")
                          #+:mt
                          (loop
                              for derivation
                              = (mt::read-derivation-from-string tree)
                              then (mt::edge-daughter derivation)
                              while (and (mt::edge-p derivation) 
                                         (mt::edge-daughter derivation))
                              do
                                (format
                                 *html-stream*
                                 "<td class=resultsDerivation>~
                                    ~(~a~)&nbsp;&nbsp;[~a]</td></tr>~%"
                                 (mt::edge-rule derivation)
                                 (mt::edge-id derivation)))
                          (format *html-stream* "</table></td>~%")
                        when (or mrs edge) do
                          (format *html-stream* "<td class=resultsMrs>~%")
                          (mrs::output-mrs1 
                           (or mrs (mrs::extract-mrs edge))
                           'mrs::html *html-stream* i)
                          (format *html-stream* "</td>~%")
                        do (format *html-stream* "</tr>")))
                   ((or (null error) (equal error ""))
                    (format
                     *html-stream* 
                     "<div id=error>~
                      <p>No result(s) were found for this input.&nbsp;&nbsp;~
                      Is it grammatical?</p>~%~
                      </div>~%"))
                   ((integerp error)
                    (format
                     *html-stream* 
                     "<div id=error>~
                      <p>The processor exhausted its search space limit ~
                      (of ~d passive edge~p);<br>~
                      try non-exhaustive processing or a shorter ~
                      (or less ambiguous) ~
                      input.</p>~%</div>~%"
                     error error))
                   ((consp error)
                    (format
                     *html-stream* 
                     "<div id=error>~
                      The following input tokens were ~
                      not recognized by the processor: ~
                      ~{&lsquo;~(~a~)&rsquo;~^ ~}.~%</div>~%"
                     error))
                   ((and rawp (stringp error))
                    (format
                     *html-stream* 
                     "<div id=error>~a.~%</div>~%"
                     (string-right-trim '(#\. #\? #\!) error)))
                   (t
                    (format
                     *html-stream* 
                     "<div id=error>~
                      The server encountered an (unexpected) error: ~
                      &lsquo;~a&rsquo;.~%</div>~%"
                     (string-right-trim '(#\. #\? #\!) error))))
                   (www-version *html-stream*)))))))))))

(defun determine-string-class (string)
  (cond
   ((search " || /" string) :token)
   ((search "|| " string) :fragment)))

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
                        for mrs = (let ((mrs (tsdb::get-field :mrs result)))
                                    (mrs::read-mrs-from-string mrs))
                        for edge = (or (tsdb::get-field :edge result)
                                       (let ((edge
                                              (if derivation
                                                (tsdb::reconstruct derivation)
                                                (lkb::make-edge
                                                 :from 0 :to 0))))
                                         (nconc result (acons :edge edge nil))
                                         (setf (lkb::edge-mrs edge) mrs)
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
        (www-doctype *html-stream*)
        (html (:html
               (www-header *html-stream* "Redwoods Tree Comparison")
               ((:body :onload "messenger()")
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
                       :size 1 :name "mode" :disabled '||
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
        (www-doctype *html-stream*)
        (html (:html
               (www-header *html-stream* "Redwoods Treebanks")
               ((:body :onload "messenger()")
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
          (www-doctype *html-stream*)
        (html 
         (:html
          (www-header *html-stream* "Redwoods Annotation Summary")
          ((:body :onload "messenger()")
           (:center
            (tsdb::analyze-trees 
             data :file *html-stream* :condition condition :format :html)
            (www-version *html-stream*)))))))))))

(defun www-doctype (stream)
  (format
   stream
   "<!DOCTYPE HTML PUBLIC ~
      \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%"))

(defun www-header (stream title &optional (name "default"))
  (let ((*html-stream* stream))
    (html (:head
           ((:meta
             :http-equiv "Content-Type" 
             :content "text/html; charset=utf-8"))
           (:title (format stream "~a" title))
           :newline
           ((:link
             :type "text/css" :rel "stylesheet"
             :href "/lkb.css"))
           :newline
           ((:link
             :type "image/gif" :rel "icon"
             :href "/icon.gif"))
           :newline
           ((:script
             :src "/lkb.js" :language "javascript" 
             :type "text/javascript"))
           :newline
           ((:script
             :src "/alttxt.js" :language "javascript" 
             :type "text/javascript"))
           :newline
           (when name
             (format stream "<script>window.name = '~a'</script>~%" name)))
          :newline)))

(defun www-version (stream)
  (format
   stream 
   "<center>~%<div id=version>[~
     <a href=\"http://www.emmtee.net/\">LOGON</a> (~a) &mdash; ~
     <a href=\"http://www.ling.uib.no/~~victoria/NorGram/\">~a</a> &mdash; ~
     ~a &mdash; <a href=\"http://www.delph-in.net/erg/\">~a</a>]</div>~%"
   (subseq mt::*version* 7 (- (length mt::*version*) 2))
   (loop
       for client in tsdb::*pvm-clients*
       for cpu = (pvm:client-cpu client)
       when (tsdb::smember :parse (pvm:cpu-task cpu))
       return (pvm:cpu-grammar cpu)
       finally (return "unknown"))
   (loop
       for client in tsdb::*pvm-clients*
       for cpu = (pvm:client-cpu client)
       when (tsdb::smember :transfer (pvm:cpu-task cpu))
       return (pvm:cpu-grammar cpu)
       finally (return "unknown"))
   (tsdb::current-grammar))
  (format
   stream
   "</center>~%<div id=\"navtxt\" class=\"navtext\" ~
         style=\"position: absolute; top: -100px; left: 0px; ~
                 visibility: hidden\"></div>"))

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
           (or host address) input readings time edges error)))))
  (defun www-warn (request string)
    (mp:with-process-lock (lock)
      (with-open-file (stream *www-log* :direction :output
                       :if-does-not-exist :create :if-exists :append)
        (let* ((socket (request-socket request))
               (address (socket:remote-host socket))
               (host (socket:ipaddr-to-hostname address)))
          (format
           stream
           "[~a] www-warn(): [~a] ~a.~%"
           (tsdb::current-time :long :pretty) 
           (or host address) string))))))

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
    (when (and (numberp n) (>= n 0) (< n (array-total-size %www-attic%)))
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
