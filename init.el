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

;; Load config for Org Mode GTD workflow
(load "org-gtd.el")

;; Load org contributionn package for task dependencies (sequential projects)
(load "org-depend.el")

;; Load org contributionn package for clearing checklists of recurring tasks
(load "org-checklist.el")

;; Load config for project management with Org Mode
(load "org-project.el")

;; Load misc Org Mode config
(load "org-misc.el")

(provide 'init)
;;; init.el ends here
