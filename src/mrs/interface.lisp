(in-package "MRS")

;;; LKB specific


(defun time-scope nil 
  (format t "~%~A" user::*sentence*) 
  (for res in user::*parse-record* 
       do 
       (let ((current-time (get-internal-run-time)))
         (format t " scopes ~A time ~A" 
                 (length (make-scoped-mrs 
                          (car (extract-mrs (list res) t))))
                 (- (get-internal-run-time) current-time)))))

(defun show-all nil 
  (format t "~%~A" user::*sentence*) 
  (for res in user::*parse-record* 
       do 
       (let ((mrsstruct (car (extract-mrs (list res) t))))
        (show-all-scoped-structures mrsstruct (make-scoped-mrs mrsstruct)))))

(defun time-first nil 
  (format t "~%~A" user::*sentence*) 
  (format t " ~A" (length (make-scoped-mrs (car (extract-mrs (list (car user::*parse-record*)) t))))))

(defun test-cache nil 
  (let ((current-cache *cache-on*))
    (format t "~%~A" user::*sentence*) 
    (for res in user::*parse-record* 
         do 
         (let* ((mrs (car (extract-mrs (list res) t)))
                (uncached-result (progn (setf *cache-on* nil)
                                        (make-scoped-mrs mrs)))
                (cached-result (progn (setf *cache-on* t)
                                      (make-scoped-mrs mrs)))
                (uncached-canonical 
                 (for res in uncached-result
                      collect
                      (canonical-bindings res)))
                (cached-canonical 
                 (for res in cached-result
                      collect
                      (canonical-bindings res))))
           (if (eql (length uncached-result)
                    (length cached-result))
               (when 
                   (for thing in uncached-canonical
                        all-satisfy
                        (if
                            (member thing cached-canonical 
                                    :test #'bindings-equivalent)
                            t
                          (format t "~%WARNING cached contains ~A" 
                                  thing)))
                 (format t " ~A" (length cached-result)))
             (format t "~%WARNING cache results differ uncached: ~A cached: ~A"
                     (length uncached-result) (length cached-result)))))
    (setf *cache-on* current-cache)))


(defun bindings-equivalent (b1 b2)
  (and (eql (length b1)
            (length b2))
       (for thing in b1
            all-satisfy
            (member thing b2 
                    :test #'equal))))

(defparameter *alex-munge* nil)

(defun show-mrs nil
  (let ((*print-circle* nil))
    (format t "~%********************")
    (format t "~%~{~A ~}" cl-user::*sentence*)
    (dolist (mrs-struct (extract-mrs *parse-record*))
      (treat-mrs mrs-struct nil))))

(defun show-mrs-full nil
  (let ((*print-circle* nil))
    (format t "~%********************")
    (format t "~%~{~A ~}" cl-user::*sentence*)
    (dolist (mrs-struct (extract-mrs *parse-record*))
      (when *alex-munge*
        (output-mrs mrs-struct 'simple))
      (treat-mrs mrs-struct nil))))

(defun treat-mrs (mrs-struct extra-param)
  (declare (ignore extra-param))
  (cond (*mrs-to-vit*
         (mrs-to-vit-convert mrs-struct))
        ((and *alex-munge* (fboundp 'alex-munge))
         (alex-munge mrs-struct))
        (*mrs-scoping*
         (check-mrs-struct mrs-struct))
        (t (output-mrs mrs-struct 'simple))))


(defvar *mrs-record* nil)

#| 
(mrs::output-mrs-after-parse *parse-record*)
|#

(defun output-mrs-after-parse (edges)
  (when (and (or *mrs-to-vit* *mrs-scoping*
               *mrs-output-p* *alex-munge*))
    (setf *mrs-record*
      (extract-mrs edges))
    (for mrs in *mrs-record* 
         do
         (format t "~%~A~%" (cl-user::parse-tree-structure (car edges)))
         (setf edges (cdr edges))
         (treat-mrs mrs t))))

#|
(in-package "CL-USER")


(defparameter *do-something-with-parse* 'mrs::show-mrs)

(defparameter *do-something-with-parse* 'mrs::time-scope)

(defparameter *do-something-with-parse* 'mrs::test-cache)

|#
