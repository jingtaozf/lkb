(pushnew :lingo *features*)
(pushnew :lkb *features*)

(load
   (make-pathname :name "lkb-mrs-package"
                  :directory (append *lkb-source-dir* (list "mrs"))))

;;; define MRS package

(in-package "MRS")

(export '(psoa-handel psoa-top-h psoa-index psoa-liszt psoa-h-cons
          psoa-message psoa-wgliszt
          rel-extra rel-type rel-sort rel-handel rel-label rel-flist
          fvpair-feature fvpair-value
          var-name var-extra var-id
          handle-var-name handle-var-extra handel-var-id
          group-var-name group-var-extra group-var-id
          hcons-scarg hcons-cands hcons-outscpd
          leq-sc-scarg leq-sc-cands leq-sc-outscpd leq-sc-relation
          whg-id-id whg-id-word whg-id-handel))


(defun vsym (str) 
  ;;; allow mrsglobals-eng file to be system independent
  (intern (string-upcase str) "USER"))

(in-package "USER")


(eval-when (load eval)
(with-compilation-unit ()
(progn 
      (dolist  (dir-and-file 
        '( (("mrs") "mrsglobals")        ; have to redefine most of these,
                                       ; but this is mostly 
                                       ; done by mrsglobals-eng 
           (("mrs") "basemrs")        ; MRS structures and printing
           (("mrs") "mrsoutput")      ; constructing MRS from parse result  
          (("mrs") "mrscorpus")         ; checking equality etc
          (("mrs") "interface")
; following two files needed for scoping - can be excluded for generation 
           (("mrs") "mrsresolve")     ; resolving scope
           (("mrs")  "mrscons")       ; constraints on scope
; following five files for mrs to vit  - can be excluded for generation 
           (("mrs") "vit")            ; VIT structures
           (("mrs") "mrs-to-vit")     ; convert MRS to VIT
           (("mrs") "time-convert")   ; temporary code for converting
                                      ; times to VIT format
                                      ; needs fixing
           (("mrs") "mrsmunge")       ; manipulate MRS via rules
                                      ; currently for vitrifying - potential
                                      ; other uses
           (("mrs") "mrsruleinput")   ; creating rules for above
                                      ; requires LKB, but outputs compiled 
                                      ; rules which can be used without LKB
;           (("mrs") "acl-mrs")        ; display etc in CLIM
                                       ; needs fixing
           (("mrs") "mrsfns")         ; from old mrsglobals
           (("mrs") "mrsglobals-eng") ; globals for LinGO grammar
           (("mrs") "lkbmrs")         ; LKB specific - redefines
                                      ; some functions
           (("mrs") "lexindex")       ; LKB specific - indexing
                                      ; entries etc on semantics
           (("mrs") "lexlookup")      ; LKB specific - retrieving
                                      ; entries etc for generation
))                 
      (let* ((dir (car dir-and-file))
             (file (cadr dir-and-file))
            (source-file (make-pathname :name file
               :directory (append *lkb-source-dir* dir)))
            (compiled-file (make-pathname :name file
                                          :directory (append *lkb-fasl-dir* dir))))
        #-mcl(compile-file source-file :output-file compiled-file)
        (load #-mcl compiled-file #+mcl source-file))))
))

;;; loading vitrification rules
(when (fboundp 'read-mrs-rule-file-aux)
    (read-mrs-rule-file-aux 
     (make-pathname :name "dan-rules.mrs"
                    :directory (append *lkb-source-dir* '("mrs" "data")))))

(in-package :cl-user)
