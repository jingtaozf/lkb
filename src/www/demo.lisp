;;; -*- Mode: Common-Lisp; Package: www; -*-

(in-package :lkb)

(defpackage :www
  (:use :net.aserve :net.html.generator :lkb :make :common-lisp))

(in-package :www)

(defparameter *www-port* 8000)

(defparameter *www-lkb-css*
  (make-pathname
   :directory (pathname-directory 
               (dir-append (get-sources-dir "www") '(:relative "www")))
   :name "lkb.css"))

(defparameter *www-maximal-number-of-results* 10)

(defun lingo-publish ()
  (start :port *www-port*)
  (publish-file :path "/lkb.css" :file *www-lkb-css*)
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
                    "howdy, just hit return in this window!"))
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
                     (member "mrs" output :test #'equal)))))
    (with-http-response (request entity)
      (with-http-body (request entity)
        (html (:html
               (:head
                (:title "LinGO English Resource Grammar On-Line")
                ((:link
                  :type "text/css" :rel "stylesheet" :href "/lkb.css")))
               (:body
                (:center
                 ((:form 
                   :action "/erg" :method "post"
                   :accept-charset "utf-8")
                  ((:input
                    :type "text" :name "input"
                    :value (or input "") :size "90%"))
                  :br
                  ((:table :border 0 :cellspacing 0)
                   (:tr
                    ((:td :class "buttons") "results:")
                    ((:td :class "buttons")
                     ((:input
                       :type :radio :name "exhaustivep"
                       :value "all" :if* exhaustivep :checked "")))
                    ((:td :class "buttons") "all")
                    ((:td :class "buttons")
                     ((:input
                       :type :radio :name "exhaustivep"
                       :value "first"
                       :if* (not exhaustivep) :checked "")))
                    ((:td :class "buttons") "first" )
                    ((:td :class "buttons")
                     "&nbsp;&nbsp;|&nbsp;&nbsp;output:")
                    ((:td :class "buttons")
                     ((:input
                       :type "checkbox" :name "output" :value "tree"
                       :if* treep :checked "")))
                    ((:td  :class "buttons") "tree")
                    ((:td :class "buttons")
                     ((:input
                       :type "checkbox" :name "output" :value "mrs"
                       :if* mrsp :checked "")))
                    ((:td :class "buttons") "mrs")))))
                (:center
                 (when (and (eq method :post) input)
                   (www-parse-input
                    input
                    :exhaustivep exhaustivep :treep treep :mrsp mrsp
                    :stream *html-stream*))))))))))

(defparameter %www-parser-lock% (mp:make-process-lock))

(defun www-parse-input (input &key exhaustivep treep mrsp stream)
  (mp:with-process-lock (%www-parser-lock%)
    (lkb::parse (lkb::split-into-words 
            (lkb::preprocess-sentence-string 
             (string-trim '(#\space #\tab #\newline) input)))
           nil
           (not exhaustivep))
    (let* ((start (first (last lkb::*parse-times*)))
           (end (first lkb::*parse-times*))
           (time (when (and (numberp start) (numberp end))
                   (/ (- end start) internal-time-units-per-second))))
      (format
       stream 
       "<div id=summary>~
        [~d of ~d solution~p~@[; processing time: ~,2f seconds~]]</div>~%~
        <br>~%"
       (min (length *parse-record*) *www-maximal-number-of-results*)
       (length *parse-record*) (length *parse-record*) time))
    (loop
        initially (format stream "<table class=results>~%")
        finally (format stream "</table>~%")
        for edge in lkb::*parse-record*
        for i from 1 to (or *www-maximal-number-of-results* 
                            (length *parse-record*))
        for mrs = (mrs::extract-mrs edge)
        do
          (format 
           stream 
           "<tr>~%<td class=navigation>~%  <table class=navigation>~%    ~
            <tr><td class=center><div class=id># ~a</div></td></tr>~%    ~
            ~:[<tr><td class=center>~
              <div class=action>TREE</div></td></tr>~%~;~]    ~
            <tr><td class=center><div class=action>AVM</div></td></tr>~%    ~
            ~:[<tr><td class=center>~
              <div class=action>MRS</div></td></tr>~%~;~]    ~
            <tr><td class=center><div class=action>SCOPE</div></td></tr>~%    ~
            </table></td>~%"
           i treep mrsp)
        when treep do
          (format stream "<td class=tree>~%")
          (lkb::edge2html edge stream)
          (format stream "</td>~%")
        when mrsp do
          (format stream "<td class=mrs>~%")
          (mrs::output-mrs1 mrs 'mrs::html stream)
          (format stream "</td>~%")
        do (format stream "</tr>"))))

(defun lookup-form-value (name query)
  (loop
      with result = nil
      for (key . value) in query
      when (string-equal key name) do (push value result)
      finally (return (if (rest result) result (first result)))))
