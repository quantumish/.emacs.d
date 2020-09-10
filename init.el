
(package-initialize) 
;; Set the load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "external/elcord.el")

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

;; Load config for word processing
(load "word-processing.el")

;; Load org contribution packages
(load "external/org-depend.el")
(load "external/org-checklist.el")

;; Load config for project management with Org Mode
(load "org-project.el")

;; Load misc Org Mode config
(load "org-misc.el")

;; Load notetaking setup
(load "org-notes.el")

;; Load experimental messages in echo area
;; (load "echo-message.el")

;; Load experimental multiline modeline
;; (load "multiline-modeline.el")

;; Load broken plugin for indication of workload for calendar entries
;; (load "material-colors.el")
;; (load "year-calendar.el")

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-frontends (quote (company-preview-frontend)))
 '(package-selected-packages
   (quote
    (smooth-scrolling evil beacon neotree rainbow-mode deft persp-mode calfw-org calfw org-roam-server org-capture-pop-frame org-cliplink idle-org-agenda company-org-roam use-package org-roam ## pandoc-mode powerthesaurus define-word yasnippet yaml-mode writeroom-mode undo-tree treemacs projectile posframe org-super-agenda olivetti modern-cpp-font-lock minibuffer-line lsp-ui haskell-mode focus flycheck diff-hl company-box cmake-mode auctex all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#f5f5f5"))))
 '(company-scrollbar-fg ((t (:background "#999999"))))
 '(company-tooltip ((t (:foreground "#333333" :background "#f5f5f5"))))
 '(company-tooltip-common ((t (:foreground "#00008b"))))
 '(company-tooltip-common-selection ((t (:foreground "#999999"))))
 '(lsp-ui-doc-background ((t :background "#fafafa")))
 '(lsp-ui-doc-header ((t :background "#f0f0f0")))
 '(lsp-ui-doc-url ((t :inherit link)))
 '(org-document-title ((t (:inherit face-salient :weight bold :height 1.25))))
 '(org-headline-done ((((class color) (class color) (min-colors 16)) (:foreground "#cfd1d1")))))
