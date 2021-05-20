;; TODO: Set up treemacs.

(use-package doom-themes
	:init
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		  doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'ewal-doom-one t)

	(doom-themes-visual-bell-config)

	;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
	;(doom-themes-treemacs-config)
	(doom-themes-org-config))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 40)
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
  (setq centaur-tabs-set-bar 'left)
  (setq x-underline-at-descent-line t)
  (defun contextual-tabs ()
	(if (and (centaur-tabs-mode-on-p) (eq (derived-mode-p 'prog-mode) nil))
		(centaur-tabs-local-mode)))
  (centaur-tabs-mode)
  :hook
  (after-change-major-mode . contextual-tabs)
  :bind
  ("H-l" . 'centaur-tabs-forward-tab)
  ("H-h" . 'centaur-tabs-backward-tab))

