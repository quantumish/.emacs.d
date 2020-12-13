
(package-initialize) 
;; Set the load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "~/.emacs.d/floobits/floobits.el")
(load "external/elcord.el")

;; Load an elegant theme
(load "sanity.el")
;; (load "elegance.el") Byeee
;; (load "solo-jazz-theme.el")
(load "themeage.el") ;; theme config

;; Load aesthetic improvements for Org Mode (icons, custom faces, etc.)
(load "org-aesthetic.el")

;; Load aesthetic improvements for coding (lsp faces, focus mode, etc.)
(load "code-aesthetic.el")

;; Load actual improvements for coding (lsp, company, so forth...)
(load "code-features.el")

;; Load config for Org Mode GTD workflow
(load "org-gtd.el")

;; Load config for word processing
(load "word-processing.el")

;; Load org contribution packages
(load "external/org-depend.el")
(load "external/org-checklist.el")

;; Load config for project management with Org Mode
(load "org-project.el")

;; Load misc Org Mode config
(load "org-misc.el")
(load "minimal.el")

;; Load notetaking setup
(load "org-notes.el")
(load "external/tag.el")
(load "tagconfig.el")

;; Load sane defaults for emacs
(load "sanity.el")

(load "mu4e.el")

(setq ivy-posframe-width 60)
(setq ivy-posframe-border-width 1)
(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)te))

;; Load experimental messages in echo area
;; (load "echo-message.el")

;; Load experimental multiline modeline
;; (load "multiline-modeline.el")

;; Load broken plugin for indication of workload for calendar entries
;; (load "material-colors.el")
;; (load "year-calendar.el")

(provide 'init)
;;; init.el ends here

