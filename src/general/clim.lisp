                                                                     
                                                                     
                                                                     
                                             
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :climxm))

(in-package :silica)


#-(or aclpc acl86win32) 
(defun mapping-table-initform ()
  (excl:ics-target-case
   (:+ics (let ((v (make-array 4 :adjustable t)))
	    (dotimes (i 4)
	      (setf (aref v i)
		(make-hash-table :test #'equal)))
	    v))
   (:-ics (make-hash-table :test #'equal))))

#-(or aclpc acl86win32) 
(defun mapping-cache-initform ()
  (excl:ics-target-case
   (:+ics (let ((v (make-array 4 :adjustable t)))
	    (dotimes (i 4)
	      (setf (aref v i)
		(cons nil nil)))
	    v))
   (:-ics (cons nil nil))))

(defun make-text-style (family face size &aux changed-p original-face)
  (unless (numberp face)
    (setf original-face face
          face (face->face-code face))) ;"Intern" the face code.
  (loop
    (let* ((family-stuff (assoc family *text-style-intern-table*))
           (face-stuff (and family-stuff (assoc face (cdr family-stuff))))
           (size-stuff (and face-stuff (assoc size (cdr face-stuff)))))
      (when size-stuff (return-from make-text-style (cdr size-stuff)))
      (multiple-value-setq (family face size changed-p original-face)
          (validate-text-style-components family face size original-face))
      (unless changed-p
        (macrolet ((ensure-stuff (stuff thing from-stuff)
                     `(unless ,stuff
                        (setf ,stuff (cons ,thing nil))
                        (with-lock-held (*text-style-lock* "Text style lock")
                          (setf ,from-stuff (nconc ,from-stuff (cons ,stuff nil)))))))
          (ensure-stuff family-stuff family *text-style-intern-table*)
          (ensure-stuff face-stuff face family-stuff)
          (ensure-stuff size-stuff size face-stuff))
        (let* ((new-style (make-text-style-1 family face size nil)))
          (setf (cdr size-stuff) new-style)
          (return-from make-text-style new-style))))))

(defmethod port-mapping-table ((port basic-port) character-set)
  (with-slots (mapping-table) port
     #+allegro
     (excl:ics-target-case
       (:-ics character-set mapping-table)
       (:+ics (let ((old-length (length mapping-table)))
                (when (>= character-set (length mapping-table))
                  (setf mapping-table (adjust-array mapping-table (1+ character-set)))
                  (dotimes (i (- (length mapping-table) old-length))
                    (setf (aref mapping-table (+ i old-length)) (make-hash-table :test #'equal)))))
              (aref mapping-table character-set)))
     #-allegro
     mapping-table))

(defmethod port-mapping-cache ((port basic-port) character-set)
  (with-slots (mapping-cache) port
     #+allegro
     (excl:ics-target-case
       (:-ics character-set mapping-cache)
       (:+ics (let ((old-length (length mapping-cache)))
                (when (>= character-set (length mapping-cache))
                  (setf mapping-cache (adjust-array mapping-cache (1+ character-set)))
                  (dotimes (i (- (length mapping-cache) old-length))
                    (setf (aref mapping-cache (+ i old-length)) (cons nil nil)))))
              (aref mapping-cache character-set)))
     #-allegro
     mapping-cache))

(defmethod (setf text-style-mapping) (mapping (port basic-port) style
                                              &optional (character-set *standard-character-set*)
                                              window)
  (declare (ignore window))
  (setq style (standardize-text-style port (parse-text-style style) character-set))
  (when (listp mapping)
    (assert (eq (first mapping) :style) ()
            "Text style mappings must be atomic font names ~
             or (:STYLE . (family face size))")
    (setf mapping (parse-text-style (cdr mapping))))
  (with-slots (allow-loose-text-style-size-mapping) port
     (let ((mapping-table (port-mapping-table port character-set))
           (mapping-cache (port-mapping-cache port character-set)))
       (without-scheduling
         (setf (car mapping-cache) nil
               (cdr mapping-cache) nil))
       (if allow-loose-text-style-size-mapping ; ### <----!!!
           (multiple-value-bind (family face size) (text-style-components style)
             (declare (ignore size))
             (with-stack-list (key family face)
               (let* ((fonts (gethash key mapping-table))
                      (old (assoc style fonts)))
                 (cond (old
                        (setf (second old) mapping))
                       (t
                        (push (list style mapping) fonts)
                        (setq fonts (sort fonts #'(lambda (e1 e2)
                                                    (< (text-style-size (first e1))
                                                       (text-style-size (first e2))))))
                        (setf (gethash (copy-list key) mapping-table)
                              fonts)))
                 mapping)))
           (setf (gethash style mapping-table) mapping)))))



(in-package :xt)

(x11::def-exported-constant lc-ctype 0)
(x11::def-exported-constant lc-all 6)

(def-foreign-call (x-supports-locale "XSupportsLocale")
    ()
  :returning (:int fixnum)
  :arg-checking nil
  :strings-convert t)

(def-foreign-call (x-set-locale-modifiers "XSetLocaleModifiers")
    ((x :foreign-address))
  :call-direct t
  :returning :foreign-address
  :arg-checking nil)

(defparameter *external-formats-to-locale-charset-alist*
              `((,(excl:find-external-format "UTF-8") . "UTF-8")
                (,(excl:find-external-format "LATIN1") . "ISO-8859-1")))

(ff:defun-foreign-callable xt-current-locale-for-acl ((display (* :void)) (xnl (* :char)) (client-data (* :void)))
  (declare (:convention :c)
           (ignore display client-data xnl))
  
  (labels ((ef-name (ef)
             (if (excl:composed-external-format-p ef)
                 (or (ef-name (excl:ef-composer-ef ef))
                     (ef-name (excl:ef-composee-ef ef)))
                 (cdr (assoc ef *external-formats-to-locale-charset-alist*))))
           (try-locale (locale)
             (setlocale lc-all locale)
             (let ((supported (x-supports-locale)))
               (unless (zerop supported)
                 (x-set-locale-modifiers "")
                 locale))))
    (let ((locale (excl:locale-name excl:*locale*))
          (encoding (ef-name (excl:locale-external-format excl:*locale*))))
      (or (if encoding
              (try-locale (format nil "~A.~A" locale encoding))
              (warn "Couldn't determine unix encoding of locale ~A's external format. Falling back to default encoding for locale." excl:*locale*))
          (try-locale (format nil "~A" locale))
          ;; didn't find any locale.
          (try-locale "C")))))

(defun initialize-toolkit (&rest args)
  (let ((context (create-application-context)))
    (when *fallback-resources*
      (xt_app_set_fallback_resources
       context
       (let ((n (length *fallback-resources*)))
	 (with-*-array (v (1+ n))
	   (dotimes (i n)
	     (setf (*-array v i)
                   (clim-utils:string-to-foreign (nth i *fallback-resources*))))
	   (setf (*-array v n) 0)
	   v))))
    (excl:ics-target-case
      (:+ics (xt_set_language_proc 0 (register-foreign-callable 'xt-current-locale-for-acl :reuse t) 0)))
    (let* ((display (apply #'make-instance 'display
			   :context context
			   args))
	   (app (apply #'app-create-shell
		       :display display
		       :widget-class 'application-shell
		       args)))
      (values context display app))))


(setf *font-list-tags*
    (make-array 4 :adjustable t :fill-pointer t
                :initial-contents
                (list
                 (excl:string-to-native "ascii")
                 (excl:string-to-native "kanji")
                 (excl:string-to-native "katakana")
                 (excl:string-to-native "gaiji"))
       
                ))

(defun export-font-list (value)
  (when (atom value)
    (setq value (list value)))
  (flet ((create-font-list-entry (font)
           (note-malloced-object
            (excl:ics-target-case
              (:+ics
               (let ((tag ""))
                 (when (consp font)
                   (setq tag (aref *font-list-tags* (car font))
                         font (cdr font)))
                 (xm_font_list_entry_create tag
                                            (etypecase font
                                              (font xm-font-is-font)
                                              (font-set xm-font-is-fontset))
                                            font)))
              (:-ics
               (xm_font_list_entry_create ""
                                          xm-font-is-font
                                          font))))))
    (let ((font-list
           (xm_font_list_append_entry
            0				; old entry
            (create-font-list-entry (car value)))))
      (dolist (font (cdr value))
        (setq font-list
              (xm_font_list_append_entry font-list
                                         (create-font-list-entry font))))
      (note-malloced-object font-list
                            #'free-font-list))))

(defmethod convert-resource-out ((parent t) (type (eql 'xm-string)) value)
  (let ((result nil))
    (flet ((extract-element (codeset start end)
             (let* ((substring (subseq value start end))
                    (element (xm_string_create_l_to_r
                              (ecase codeset
                                ((0 2) (lisp-string-to-string8 substring))
                                ((1 3) (lisp-string-to-string16 substring)))
                              (aref *font-list-tags* codeset))))
               (tk::add-widget-cleanup-function parent
                                                #'destroy-generated-xm-string
                                                element)
               (let ((temp (if result
                               (xm_string_concat result element)
			       (xm_string_copy element))))
                 (tk::add-widget-cleanup-function parent
                                                  #'destroy-generated-xm-string
                                                  temp)
                 (setq result temp)))))
      (declare (dynamic-extent #'extract))
      (partition-compound-string value #'extract-element)
      (or result
          *empty-compound-string*
          (setq *empty-compound-string*
                (xm_string_create_l_to_r (clim-utils:string-to-foreign "")
                                         (clim-utils:string-to-foreign 
                                          xm-font-list-default-tag)))))))

(in-package :xm-silica)

(defvar *default-fallback-font* "fixed")

(defun list-fonts-by-registry (display)
  (let* ((fonts (tk::list-font-names display "-*-*-*-*-*-*-*-*-*-*-*-*-*-*"))
         (encoding-hash (make-hash-table :test #'equal)))
    (dolist (font fonts)
      (let ((font* (disassemble-x-font-name font)))
        (push font (gethash (last font* 2) encoding-hash))))
    encoding-hash))

(defun find-font-with-properties (fonts weight slant)
  (or (find (list weight slant) fonts
            :test #'equal
            :key (lambda (font)
                   (let ((font* (disassemble-x-font-name font)))
                     (list (nth 3 font*) (nth 4 font*)))))
      (first fonts)))

(defun font-name-of-aliased-font (display fontname)
  (excl:with-native-string (nfn fontname)
    (let ((font (x11:xloadqueryfont display nfn)))      
      (unless (zerop font)
        (unwind-protect
            (loop for i from 0 below (x11:xfontstruct-n-properties font)
                  for fontprop = (+ (* i 2 #-64bit 4 #+64bit 8)
                                  (x11:xfontstruct-properties font))
                  when (eql x11:xa-font (x11:xfontprop-name fontprop))
                    do (return (values (excl:native-to-string 
                                        (x11:xgetatomname display
                                                          (x11:xfontprop-card32 fontprop))))))
          (x11:xfreefont display font))))))


(defmethod initialize-xlib-port ((port xt-port) display)
  (let* ((screen (x11:xdefaultscreen display))
	 ;;-- This is a property of the graft
	 (screen-pixels-per-inch
	  (* 25.4 (/ (x11::xdisplayheight display screen)
		     (x11:xdisplayheightmm display screen)))))
    (labels ((font->text-style (font family)
               (flet ((parse-token (token)
                        (if token
                            (parse-integer token)
                            (return-from font->text-style nil))))
                 (let* ((tokens (disassemble-x-font-name font))
                        (italic (member (nth 4 tokens) '("i" "o") :test #'equalp))
                        (bold (equalp (nth 3 tokens) "bold"))
                        (face (if italic
                                  (if bold '(:bold :italic) :italic)
                                  (if bold :bold :roman)))
                        (pixel-size (parse-token (nth 7 tokens)))
                        (point-size (parse-token (nth 8 tokens)))
                        (y-resolution (parse-token (nth 10 tokens)))
                        (average-width (parse-token (nth 12 tokens)))
                        (corrected-point-size (* (float point-size)
                                                 (/ y-resolution
                                                    screen-pixels-per-inch))))
                   (unless (and (not *use-scalable-fonts*)
                                (or (eql pixel-size 0)
                                    (eql point-size 0)
                                    (eql average-width 0)))
                     (make-text-style family face (/ corrected-point-size 10))))))
             (load-1-charset (character-set fallback families)
               (let* ((matchesp nil) ;do any non-fallback fonts match?
                      (fallback-matches-p ;any fallback matches?
                       (not (null (tk::list-font-names display fallback))))
                      (fallback-loadable-p ;fallback actually loadable?
                       (and fallback-matches-p
                            (excl:with-native-string (nfn fallback)
                              (let ((x (x11:xloadqueryfont display nfn)))
                                (if (not (zerop x))
                                    (progn
                                      (x11:xfreefont display x)
                                      t)
                                    nil))))))
                 (dolist (per-family families)
                   (destructuring-bind (family &rest patterns) per-family
                     (dolist (font-pattern patterns)
                       (dolist (xfont (tk::list-font-names display font-pattern))
                         ;; this hack overcomes a bug with hp's scalable fonts
                         (unless (find #\* xfont)
                           (setf matchesp t) ;there was at least one match
                           (let ((text-style (font->text-style xfont family)))
                             ;; prefer first font satisfying this text style, so
                             ;; don't override if we've already defined one.
                             (when text-style
                               (unless (text-style-mapping-exists-p
                                        port text-style character-set t)
                                 (setf (text-style-mapping port text-style
                                                           character-set)
                                       xfont)))))))))
                 ;; Set up the fallback if it looks like there is one, and
                 ;; complain if things look bad.  Things look bad if there were
                 ;; matches but the fallback is not loadable.  If there were
                 ;; no matches then don't complain even if there appears to be
                 ;; something wrong with the fallback, just silently don't load it
                 ;; (and thus define no mappings for the character set).
                 (cond
                   (fallback-loadable-p	;all is well
                    (setf (text-style-mapping port *undefined-text-style*
                                              character-set)
                          fallback))
                   ((and matchesp fallback-matches-p)
                    (warn "Fallback font ~A, for character set ~A, matches with XListFonts,
but is not loadable by XLoadQueryFont.  Something may be wrong with the X font
setup."
                          fallback character-set))
                   (matchesp
                    (warn "Fallback font ~A not loadable for character set ~A."
                          fallback character-set))))))
      (let ((charset-number 0)
            (done-registries ()))
        (dolist (per-charset *xt-font-families*)
          (destructuring-bind (character-set fallback &rest families) per-charset
            (load-1-charset character-set fallback families)
            (setf charset-number (max charset-number character-set))
            (dolist (family families)
              (pushnew (last (disassemble-x-font-name (second family)) 2) done-registries
                       :test #'equal))))
        ;; Now setup font mappings of fonts that the user has
        ;; installed, but we don't know anything about (especially no
        ;; convenient font aliases).
        ;; Since we don't have any font alias names to rely on, we use
        ;; the "fixed" alias to find out at least a sensible default
        ;; weight and slant.
        (let* ((default-fallback (disassemble-x-font-name (font-name-of-aliased-font display *default-fallback-font*)))
               (weight (nth 3 default-fallback))
               (slant (nth 4 default-fallback)))
          (loop for character-set from (1+ charset-number) 
                for encoding being the hash-keys of (list-fonts-by-registry display) using (hash-value fonts)
                for fallback-font = (find-font-with-properties fonts weight slant)
                for default-font-match-string = (format nil "-*-*-*-*-*-*-*-*-*-*-*-*-~A-~A" (first encoding) (second encoding))
                do (unless (member encoding done-registries :test #'equal)
                     (vector-push-extend (excl:string-to-native
                                          (format nil "~A-~A" (first encoding) (second encoding)))
                                         tk::*font-list-tags*)
                     (load-1-charset character-set fallback-font
                                     `((:fix ,default-font-match-string)
                                       (:sans-serif ,default-font-match-string)
                                       (:serif ,default-font-match-string))))))
        )))
  (setup-stipples port display))

(defmethod text-style-mapping :around
	   ((port xt-port) text-style
	    &optional (character-set *standard-character-set*) window)
  (declare (ignore window))
  (if character-set
      (let ((mapping (call-next-method)))
	(if (stringp mapping)
	    (setf (text-style-mapping port text-style character-set)
	      (find-named-font port mapping character-set))
	  mapping))
    (let ((mappings nil))
      (dotimes (c (length (slot-value port 'silica::mapping-table)))  ; XXX: ugly. prettify.
        (let ((mapping (text-style-mapping port text-style c)))
	  (when mapping
            (push (cons c mapping) mappings))))
      (reverse mappings))))

(defmethod restart-port ((port xt-port))
  (let ((process (port-process port)))
    (when process
      (clim-sys:destroy-process process))
    (setq process
      (mp:process-run-function
       (list :name (format nil "CLIM Event Dispatcher for ~A"
			   (port-server-path port))
	     :priority 1000
             :initial-bindings `((excl:*locale* . ',excl:*locale*)))
       #'port-event-loop port))
    (setf (getf (mp:process-property-list process) :no-interrupts) t)
    (setf (port-process port) process)))

(in-package :clim-utils)

(eval-when (compile)
  (ff:def-foreign-call (_malloc "malloc")
      ((data :int))
    :call-direct t
    :arg-checking nil
    :returning :foreign-address)
  (ff:def-foreign-call (_free "free")
      ((data (* :char) simple-string))
    :call-direct t
    :strings-convert nil
    :arg-checking nil
    :returning :void))

(defun string-to-foreign (string &optional address)
  "Convert a Lisp string to a C string, by copying."
  (declare (optimize (speed 3))
                (type string string)
                (type integer address))
  (unless (stringp string)
    (excl::.type-error string 'string))
  (if address
      (excl:string-to-native string :address address)
      (let* ((octets (excl:string-to-octets string :null-terminate t))
             (length (length octets)))
        (declare (optimize (safety 0))
                      (type fixnum length))
        (setf address (_malloc length))
        (dotimes (i length)
          (declare (fixnum i))
          (setf (sys:memref-int address 0 i :unsigned-byte)
                (aref octets i)))
        #+clim-utils::extra-careful(let ((re-char-ed (excl:octets-to-string octets)))
          (assert (equal re-char-ed string)
                  (re-char-ed string)
                  "string isn't equal to re-chared octets~%~S~%~S!" string re-char-ed))))
  
  #+clim-utils::extra-careful(let ((re-lisped (excl:native-to-string address)))
    (assert (equal re-lisped string)
            (re-lisped string)
            "string isn't equal to re-chared foreign mem ~%~S~%~S!" string re-lisped))
  address)
