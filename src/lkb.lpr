;;; -*- lisp-version: "5.0 [Windows/x86] (8/29/98 11:02)" common-graphics: "1.323.2.84" -*-

(in-package :common-graphics-user)

(DEFPACKAGE :COMMON-LISP-USER (:EXPORT))

(DEFINE-PROJECT :NAME 'COMMON-LISP-USER::LKB
  :APPLICATION-TYPE (INTERN "Standard EXE" (FIND-PACKAGE :KEYWORD))
  :MODULES (LIST (MAKE-INSTANCE 'MODULE :NAME "mrs\\for.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "main\\initializations.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\globals.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\graphics.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\misc.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\topmenu.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\dialog.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\types.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\dag.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\yadu.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\gen.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\structs.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\user-fns.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\marks.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\checktypes.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\leaf.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\lex.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\rules.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\parse.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\generate.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\morph.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\check-unif.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-general\\menus.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-general\\outputfs.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-general\\outputtdfs.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-general\\outputsrc.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-general\\toplevel.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-paths\\typeinput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-paths\\lexinput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-paths\\ruleinput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-paths\\pathout.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-tdl\\tdltypeinput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-tdl\\tdloutput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-tdl\\tdlruleinput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-tdl\\tdllexinput.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-lilfes\\lilout.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\fsclasses.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\activefs.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\tree.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\parseout.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\chartout.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\listout.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "acl-cg_specific\\emacs.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME
                                "io-general\\tree-nodes.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-general\\utils.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "io-general\\graph.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\batch-check.lsp")
                 (MAKE-INSTANCE 'MODULE :NAME "main\\debug.lsp"))
  :PROJECTS NIL
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
  :ON-INITIALIZATION 'COMMON-LISP-USER::SET-UP-LKB-INTERACTION)

;; End of Project Definition
