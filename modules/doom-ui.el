;; TODO: Set up treemacs.

(use-package doom-themes
	:init
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		  doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'ewal-doom-one t)

	(doom-themes-visual-bell-config)

	;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
	(doom-themes-org-config))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-encoding nil)
  (doom-modeline-mode))

;; TODO: Contextual solaire
(use-package solaire-mode
  :hook
  (prog-mode . solaire-mode))

(use-package centaur-tabs
  :init
  (setq centaur-tabs-height 16)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-icon-scale-factor 0.7)
  (setq centaur-tabs-set-bar 'left)
  (setq x-underline-at-descent-line t)
  (defun contextual-tabs ()
	(if (and (centaur-tabs-mode-on-p) (eq (derived-mode-p 'prog-mode) nil))
		(centaur-tabs-local-mode)))
  (defun centaur-tabs-hide-tab (x)
	(let ((name (format "%s" x)))
	  (or
	   (window-dedicated-p (selected-window))
	   (string-match-p (regexp-quote "<") name)
	   (string-prefix-p "*lsp" name)
	   (string-prefix-p "*Compile-Log*" name)
	   (string-prefix-p "*company" name)
	   (string-prefix-p "*compilation" name)
	   (string-prefix-p "*Help" name)
	   (string-prefix-p "*straight" name)
	   (string-prefix-p "*Flycheck" name)
	   (string-prefix-p "*tramp" name)
	   (string-prefix-p "*help" name)
	   (and (string-prefix-p "magit" name)
			(not (file-name-extension name)))
	   )))
  (setq centaur-tabs-hide-tab-function 'centaur-tabs-hide-tab)
  (centaur-tabs-mode)
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (compilation-mode . centaur-tabs-local-mode)
  (dashboard-mode . centaur-tabs-local-mode)
  :bind
  ("H-l" . 'centaur-tabs-forward-tab)
  ("H-h" . 'centaur-tabs-backward-tab))

(use-package treemacs
  :after doom-themes
  :init
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq treemacs-width 30)
  :bind
  ("C-c t" . treemacs))

										; :hook
  ; (treemacs-mode . (lambda () (set-header-line 50))))

(use-package treemacs-all-the-icons
  :after treemacs
  :init
  (treemacs-load-theme "all-the-icons"))

(use-package olivetti
  :hook
  (prog-mode . (lambda () (olivetti-mode) (determine-olivetti))))
