(in-package :common-lisp-user)

;;;
;;; arrange for the following things to happen
;;;
;;;   - load local `loadup' library as suitable for [incr tsdb()] code;
;;;   - reload `defsystem' to gracefully circumvent lack of compiler;
;;;   - load [incr tsdb()] code and suitable grammar.
;;;

(let* ((root (pathname-directory *load-truename*)))
  (load (dir-and-name root "loadup.lisp"))
  (load (dir-and-name root "defsystem.lisp")))
(load-system "tsdb")
