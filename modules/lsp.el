(use-package lsp-mode
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-completion-show-detail nil)
  (setq-default lsp-enable-indentation nil)
  (setq-default lsp-enable-on-type-formatting nil)
  :commands lsp)

(use-package flycheck  
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  
  (set-face-attribute 'flycheck-error nil :underline '(:color "#265087"))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "#43709e"))
  (set-face-attribute 'flycheck-info nil :underline t)
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00011100
			#b00111110
			#b00111110
			#b00111110
			#b00011100
			#b00000000
			#b00000000
			#b00000000
			#b00000000
			#b00000000))
  (let ((bitmap 'my-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-error
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-warning
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :error-list-face 'flycheck-error-list-info
      :fringe-face 'flycheck-fringe-info))
  :hook ((c++-mode . lsp)
		 (c-mode . lsp)
		 (python-mode . lsp)
		 (js-mode . lsp)
		 (typescript-mode . lsp)))

;; TODO Dive deeper into LSP

(use-package lsp-ui
  :after lsp
  :init
  (setq lsp-ui-doc-delay 5)
  :config
  (set-face-attribute 'lsp-ui-doc-background nil
					  :background "#0b0f16")
  :hook () ((lsp-mode . lsp-ui-mode)))

(add-hook 'lsp-mode-hook (lambda () (set-window-fringes nil 15 0)))
(add-hook 'lsp-mode-hook (lambda () (header-line-spacious-dark)))
