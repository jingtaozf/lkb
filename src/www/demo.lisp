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

(defun initialize ()
  (start :port *www-port* :external-format (excl:crlf-base-ef :utf-8))
  (publish-file :path "/lkb.css" :file *www-lkb-css*)
  (publish-file :path "/lkb.js" :file *www-lkb-js*)
  (publish :path "/erg"
    :content-type "text/html"
    :function #'(lambda (request entity) (www-run request entity))))

(defun www-run (request entity)
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
                    :value (or input "") :size "120"))
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
                   (www-parse-input
                    input
                    :exhaustivep exhaustivep :treep treep :mrsp mrsp
                    :request request :stream *html-stream*))))))))))

(defparameter %www-parse-lock% (mp:make-process-lock))

(defparameter %www-visualize-lock% (mp:make-process-lock))

(defparameter %www-log-lock% (mp:make-process-lock))

(defun www-parse-input (input &key exhaustivep treep mrsp request stream)
  (let* ((result 
          (mp:with-process-lock (%www-parse-lock%)
            (tsdb::parse-item 
             input 
             :exhaustive exhaustivep 
             :edges *www-maximal-number-of-edges*)))
         (readings (tsdb::get-field :readings result))
         (time (tsdb::get-field :tcpu result))
         (time (and (numberp time) (/ time 1000)))
         (edges (tsdb::get-field :pedges result))
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
    (when request (www-log request input readings time edges error))
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
       time edges edges)
      (mp:with-process-lock (%www-visualize-lock%)
        (loop
            initially
              (format 
               stream 
               "<form action=\"/lkb/browse\" method=post
                      accept-charset=\"utf-8\">~%~
                <table class=results>~%")
            finally (format stream "</table></form>~%")
            with results = (tsdb::get-field :results result)
            for i from 1 to (or *www-maximal-number-of-results* readings)
            for result in results
            for derivation = (tsdb::get-field :derivation result)
            for edge = (tsdb::reconstruct derivation)
            for mrs = (mrs::extract-mrs edge)
            do
              (format 
               stream 
               "<tr>~%<td class=navigation>~%  <table class=navigation>~%    ~
                <tr><td class=center>~%~
                <div class=navigation># ~a</div></td></tr>~%    ~
                <tr><td class=center>~
                <input type=checkbox name=selection value=\"~a\">~
                </td></tr>~%  ~
                </table></td>~%"
               i i)
            when treep do
              (format stream "<td class=tree>~%")
              (lkb::edge2html edge stream)
              (format stream "</td>~%")
            when mrsp do
              (format stream "<td class=mrs>~%")
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
    (format 
     stream 
     "<div id=version>[ERG: ~a &mdash; LKB CVS: ~a]</div>~%"
     (tsdb::current-grammar) 
     (subseq lkb::*cvs-version* 6 (- (length lkb::*cvs-version*) 2)))))
     

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
