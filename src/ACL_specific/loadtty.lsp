;;; Loading the lkb
;;; tty interface - main functionality only

(defparameter *lkb-system-version* :main)

(defparameter *lkb-source-dir* 
               '(:absolute "eon" "e2" "users" "aac" "lkb99-expt"))


(defparameter *lkb-fasl-dir* 
               '(:absolute "eon" "e2" "users" "aac" "lkb99-expt" "fasl"))

(defparameter *psorts-temp-file* 
  (make-pathname :name "templex" 
                 :directory *lkb-source-dir*)
   "a temporary file for the lexicon")

(import '(enable-type-interactions disable-type-interactions))


(eval-when (load eval)
(excl:without-package-locks
(progn 
      (dolist  (dir-and-file 
      '( ("main" "for")         ; duplicate useful Procyon CL for loops
                           ; generic CL
         ("io-general" "utils")
         ("io-general" "tty") ; tty interface
         ("main" "types")       ; generic CL
         ("main" "dag")         ; generic CL
         ("main" "yadu")        ; generic CL
         ("main" "gen")         ; generic CL
         ("main" "structs")     ; generic CL
         ("main" "globals")     ; generic CL
         ("main" "checktypes")  ; generic CL
         ("main" "marks")       ; generic CL
         ("io-paths" "typeinput")   ; generic CL
         ("io-general" "outputfs")   ; generic CL - calls some graphics
         ("io-general" "outputtdfs")   ; generic CL
         ("io-paths" "lexinput")    ; generic CL
         ("main" "lex")         ; generic CL
         ("main" "rules")       ; generic CL
         ("io-paths" "ruleinput")       ; generic CL
         ("main" "parse")       ; generic CL
         ("main" "morph")       ; generic CL
         ("main" "check-unif")  ; generic CL
         ("io-tdl" "tdltypeinput")
         ("io-tdl" "tdloutput")
         ("io-tdl" "tdlruleinput")
         ("io-tdl" "tdllexinput")
         ))
      (let* ((dir (car dir-and-file))
             (file (cadr dir-and-file))
             (compiled-file (make-pathname :name file :type "fasl"
               :directory (append *lkb-fasl-dir* (list dir)))))
          (load compiled-file))))))













