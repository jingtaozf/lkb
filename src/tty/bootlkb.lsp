;;; Compiling the lkb
;;; tty interface - main functionality only

(in-package :cl-user)

#+allegro
(eval-when (compile load eval)
   (shadow '(defmacro defstruct)))

#+allegro
(eval-when (compile load eval)
   (lisp:defmacro defmacro (&rest args)
      `(eval-when (compile load eval)
          (lisp:defmacro ,@args)))
    (defmacro defstruct (&rest args)
      `(eval-when (compile load eval)
          (lisp:defstruct ,@args))))


;;(defparameter *lkb-source-dir* '(:absolute "Macintosh HD" "lkb99"))

;;(defparameter *lkb-fasl-dir* 
;;'(:absolute "Macintosh HD" "lkb99" "fasl"))

(defparameter *lkb-source-dir* 
  (butlast (pathname-directory *load-truename*)))

(defparameter *lkb-fasl-dir* 
  (append *lkb-source-dir* '("fasl")))

(defparameter *psorts-temp-file* 
  (make-pathname :name "templex" 
                 :directory *lkb-source-dir*)
   "a temporary file for the lexicon")

(defpackage "MRS")

(eval-when (eval)
   (load
      (make-pathname :name "for" :type "lsp"
                     :directory (append *lkb-source-dir* (list "main")))
      :verbose nil :print nil))

(eval-when (load eval)
(#+allegro excl:without-package-locks
 #-allegro progn
(with-compilation-unit ()
    (dolist  (dir-and-file 
      '( ("main" "for")         ; duplicate useful Procyon CL for loops
                           ; generic CL
         ("main" "types")       ; generic CL
         ("main" "dag")         ; generic CL
         ("main" "yadu")        ; generic CL
         ("main" "gen")         ; generic CL
         ("main" "structs")     ; generic CL
         ("main" "globals")     ; generic CL
         ("main" "marks")       ; generic CL
         ("main" "checktypes")           ; generic CL
         ("main" "leaf")
         ("io-paths" "typeinput")   ; generic CL
         ("io-general" "outputfs")   ; generic CL - calls some graphics
         ("io-general" "outputtdfs")   ; generic CL
         ("io-paths" "lexinput")    ; generic CL
         ("io-tdl" "tdltypeinput")
         ("io-tdl" "tdloutput")
         ("io-tdl" "tdlruleinput")
         ("io-tdl" "tdllexinput")
         ("main" "lex")         ; generic CL
         ("main" "rules")       ; generic CL
         ("io-paths" "ruleinput")       ; generic CL
         ("main" "parse")       ; generic CL
         ("main" "generate")       ; generic CL
#+allegro("ACL_specific" "emacs")       ; dialect specific - emacs interface
         ("main" "morph")       ; generic CL
         ("main" "check-unif")  ; generic CL
         ("main" "batch-check")
         ("main" "lkb-tsdb")    ; generic CL
         ("io-general" "tree-nodes")
         ("io-general" "utils")
         ("tty" "tty")
         ))
      (let ((dir (car dir-and-file))
            (file (cadr dir-and-file)))
      (compile-file  
            (make-pathname :name file :type "lsp"
               :directory (append *lkb-source-dir* (list dir)))
            :output-file 
            (merge-pathnames
               (make-pathname :name file :type "fasl"
               :directory (append *lkb-fasl-dir* (list dir))))))))))
