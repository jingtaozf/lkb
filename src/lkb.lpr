;;; -*- lisp-version: "5.0 [Windows/x86] (8/29/98 11:02)" common-graphics: "1.323.2.84" -*-

(in-package :common-graphics-user)

(defpackage :common-lisp-user (:export))

(define-project :name 'common-lisp-user::lkb
  :application-type (intern "Standard EXE" (find-package :keyword))
  :modules (list (make-instance 'module :name "version.lsp")
                 (make-instance 'module :name "mrs\\for.lsp")
                 (make-instance 'module :name "mrs\\for.lsp")
                 (make-instance 'module :name
                                "main\\initializations.lsp")
                 (make-instance 'module :name "main\\globals.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\graphics.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\misc.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\topmenu.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\dialog.lsp")
                 (make-instance 'module :name "main\\types.lsp")
                 (make-instance 'module :name "main\\dag.lsp")
                 (make-instance 'module :name "main\\yadu.lsp")
                 (make-instance 'module :name "main\\gen.lsp")
                 (make-instance 'module :name "main\\structs.lsp")
                 (make-instance 'module :name "main\\user-fns.lsp")
                 (make-instance 'module :name "main\\marks.lsp")
                 (make-instance 'module :name "main\\checktypes.lsp")
                 (make-instance 'module :name "main\\cdb.lsp")
                 (make-instance 'module :name "main\\leaf.lsp")
                 (make-instance 'module :name "main\\lex.lsp")
                 (make-instance 'module :name "main\\clex.lsp")
                 (make-instance 'module :name "main\\rules.lsp")
                 (make-instance 'module :name "main\\parse.lsp")
                 (make-instance 'module :name "main\\active.lsp")
                 (make-instance 'module :name "main\\morph.lsp")
                 (make-instance 'module :name "main\\check-unif.lsp")
                 (make-instance 'module :name "io-general\\menus.lsp")
                 (make-instance 'module :name "io-general\\async.lsp")
                 (make-instance 'module :name
                                "io-general\\outputfs.lsp")
                 (make-instance 'module :name
                                "io-general\\outputtdfs.lsp")
                 (make-instance 'module :name
                                "io-general\\outputsrc.lsp")
                 (make-instance 'module :name
                                "io-general\\toplevel.lsp")
                 (make-instance 'module :name
                                "io-paths\\typeinput.lsp")
                 (make-instance 'module :name "io-paths\\lexinput.lsp")
                 (make-instance 'module :name
                                "io-paths\\ruleinput.lsp")
                 (make-instance 'module :name "io-paths\\pathout.lsp")
                 (make-instance 'module :name
                                "io-tdl\\tdltypeinput.lsp")
                 (make-instance 'module :name "io-tdl\\tdloutput.lsp")
                 (make-instance 'module :name
                                "io-tdl\\tdlruleinput.lsp")
                 (make-instance 'module :name
                                "io-tdl\\tdllexinput.lsp")
                 (make-instance 'module :name "io-lilfes\\lilout.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\fsclasses.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\activefs.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\tree.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\parseout.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\chartout.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\listout.lsp")
                 (make-instance 'module :name
                                "acl-cg_specific\\emacs.lsp")
                 (make-instance 'module :name
                                "io-general\\tree-nodes.lsp")
                 (make-instance 'module :name "io-general\\utils.lsp")
                 (make-instance 'module :name "io-general\\graph.lsp")
                 (make-instance 'module :name "main\\batch-check.lsp")
                 (make-instance 'module :name "main\\debug.lsp"))
  :projects nil
  :libraries nil
  :package-name "COMMON-LISP-USER"
  :main-form nil
  :compilation-unit t
  :verbose nil
  :program-name "Allegro.Program"
  :readable-program-name "Allegro Program"
  :runtime-modules '(:cg :drag-and-drop :lisp-widget
                     :multi-picture-button :common-control
                     :edit-in-place :outline :grid :lisp-group-box
                     :header-control :progress-indicator-control
                     :common-status-bar :tab-control :trackbar-control
                     :up-down-control :dde :mci :carets :hotspots
                     :menu-selection :choose-list :directory-list
                     :color-dialog :find-dialog :font-dialog
                     :string-dialog :yes-no-list-dialog
                     :list-view-control :ole :ole-server :aclwin302)
  :help-file-module (make-instance 'build-module :name "")
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:compiler :top-level)
  :build-flags '(:exit-after-build :allow-debug)
  :old-space-size 256000
  :new-space-size 6144
  :on-initialization 'common-lisp-user::set-up-lkb-interaction)

;; End of Project Definition
