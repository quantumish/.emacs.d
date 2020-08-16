;; Set the load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Load sane defaults for emacs
(load "sanity.el")

;; Load an elegant theme
(load "elegance.el")

;; Load aesthetic improvements for Org Mode (icons, custom faces, etc.)
(load "org-aesthetic.el")

;; Load aesthetic improvements for coding (lsp faces, focus mode, etc.)
(load "code-aesthetic.el")

;; Load actual improvements for coding (lsp, company, so forth...)
(load "code-features.el")

;; Load config for Org Mode GTD workflow
(load "org-gtd.el")

;; Load org contribution packages
(load "external/org-depend.el")
(load "external/org-checklist.el")

;; Load config for project management with Org Mode
(load "org-project.el")

;; Load misc Org Mode config
(load "org-misc.el")

;; Load experimental messages in echo area
(load "echo-message.el")

;; Load experimental multiline modeline
;; (load "multiline-modeline.el")

;; Load broken plugin for indication of workload for calendar entries
;; (load "material-colors.el")
;; (load "year-calendar.el")

(provide 'init)
;;; init.el ends here
