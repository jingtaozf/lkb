;;; -*- lisp-version: "5.0 [Windows/x86] (8/29/98 11:02)" common-graphics: "1.323.2.84" -*-

(in-package :common-graphics-user)

(DEFPACKAGE :COMMON-LISP-USER (:EXPORT))

(DEFINE-PROJECT :NAME 'MRS
  :APPLICATION-TYPE (INTERN "Standard EXE" (FIND-PACKAGE :KEYWORD))
  :MODULES (LIST (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrs-package.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "mrs\\lkb-interface.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrsglobals.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\extraglobals.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\basemrs.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrsoutput.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrscorpus.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\interface.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrsfns.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrsresolve.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrscons.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\vit.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrs-to-vit.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\cheapscope.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\time-convert.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrsmunge.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\mrsruleinput.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\lkbmrs.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\lexindex.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\lexlookup.lisp")
                 (MAKE-INSTANCE 'MODULE :NAME "mrs\\after.lisp"))
  :PROJECTS (LIST (MAKE-INSTANCE 'PROJECT-MODULE :NAME
                                 "d:\\newlkb\\src\\lkb"))
  :LIBRARIES NIL
  :PACKAGE-NAME "COMMON-LISP-USER"
  :MAIN-FORM NIL
  :COMPILATION-UNIT T
  :VERBOSE NIL
  :PROGRAM-NAME "Allegro.Program"
  :READABLE-PROGRAM-NAME "Allegro Program"
  :RUNTIME-MODULES '(:CG :DRAG-AND-DROP :LISP-WIDGET
                     :MULTI-PICTURE-BUTTON :COMMON-CONTROL
                     :EDIT-IN-PLACE :OUTLINE :GRID :LISP-GROUP-BOX
                     :HEADER-CONTROL :PROGRESS-INDICATOR-CONTROL
                     :COMMON-STATUS-BAR :TAB-CONTROL :TRACKBAR-CONTROL
                     :UP-DOWN-CONTROL :DDE :MCI :CARETS :HOTSPOTS
                     :MENU-SELECTION :CHOOSE-LIST :DIRECTORY-LIST
                     :COLOR-DIALOG :FIND-DIALOG :FONT-DIALOG
                     :STRING-DIALOG :YES-NO-LIST-DIALOG
                     :LIST-VIEW-CONTROL :OLE :OLE-SERVER :ACLWIN302)
  :HELP-FILE-MODULE (MAKE-INSTANCE 'BUILD-MODULE :NAME "")
  :SPLASH-FILE-MODULE (MAKE-INSTANCE 'BUILD-MODULE :NAME "")
  :ICON-FILE-MODULE (MAKE-INSTANCE 'BUILD-MODULE :NAME "")
  :INCLUDE-FLAGS '(:COMPILER :TOP-LEVEL)
  :BUILD-FLAGS '(:EXIT-AFTER-BUILD :ALLOW-DEBUG)
  :OLD-SPACE-SIZE 256000
  :NEW-SPACE-SIZE 6144
  :AFTER-FUNCTIONS (LIST 'QUOTE '(AFTER-MRS-PROJECT-LOAD))
  :ON-INITIALIZATION 'COMMON-LISP-USER::SET-UP-LKB-INTERACTION)

;; End of Project Definition
