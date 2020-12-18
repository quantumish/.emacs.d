
;; (package-initialize) 
;; ;; Set the load path
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; (load "~/.emacs.d/floobits/floobits.el")
;; (load "external/elcord.el")

;; ;; Load an elegant theme
;; (load "sanity.el")
;; ;; (load "elegance.el") Byeee
;; ;; (load "solo-jazz-theme.el")
;; (load "themeage.el") ;; theme config

;; ;; Load aesthetic improvements for Org Mode (icons, custom faces, etc.)
;; (load "org-aesthetic.el")

;; ;; Load aesthetic improvements for coding (lsp faces, focus mode, etc.)
;; (load "code-aesthetic.el")

;; ;; Load actual improvements for coding (lsp, company, so forth...)
;; (load "code-features.el")

;; ;; Load config for Org Mode GTD workflow
;; (load "org-gtd.el")

;; ;; Load config for word processing
;; (load "word-processing.el")

;; ;; Load org contribution packages
;; (load "external/org-depend.el")
;; (load "external/org-checklist.el")

;; ;; Load config for project management with Org Mode
;; (load "org-project.el")

;; ;; Load misc Org Mode config
;; (load "org-misc.el")
;; (load "minimal.el")

;; ;; Load notetaking setup
;; (load "org-notes.el")
;; (load "external/tag.el")
;; (load "tagconfig.el")

;; ;; Load sane defaults for emacs
;; (load "sanity.el")

;; (load "mu4e.el")

;; (setq ivy-posframe-width 60)
;; (setq ivy-posframe-border-width 1)
;; (setq ivy-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)te))

;; ;; Load experimental messages in echo area
;; ;; (load "echo-message.el")

;; ;; Load experimental multiline modeline
;; ;; (load "multiline-modeline.el")

;; ;; Load broken plugin for indication of workload for calendar entries
;; ;; (load "material-colors.el")
;; ;; (load "year-calendar.el")
(org-babel-load-file "~/.emacs.d/config.org")
(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" default))
 '(doom-modeline-mode t)
 '(package-selected-packages
   '(selectrum marginalia artbollocks-mode writegood-mode company-ngram exwm yasnippet yaml-mode xterm-color writeroom-mode wakatime-mode use-package smooth-scrolling smartparens rainbow-mode projectile powerthesaurus pfuture persp-mode pdf-tools pandoc-mode org-super-agenda org-roam-server org-recur org-dashboard org-cliplink org-capture-pop-frame olivetti ns-auto-titlebar neotree modern-cpp-font-lock minibuffer-line mini-frame magit lsp-ui lsp-pyright lsp-jedi ivy-todo ivy-prescient ivy-posframe ivy-omni-org ivy-mpdel idle-org-agenda haskell-mode goto-chg google-c-style focus flycheck floobits eyebrowse expand-region emojify easy-kill doom-themes doom-modeline diff-hl deft define-word dashboard crux counsel-osx-app counsel company-org-roam company-lsp company-box color-identifiers-mode cmake-mode ccls calfw-org calfw beacon auctex all-the-icons-ivy ace-window)))
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
 '(org-headline-done ((((class color) (class color) (min-colors 16)) (:foreground "#cfd1d1")))))
