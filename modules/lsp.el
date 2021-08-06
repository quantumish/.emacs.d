(use-package lsp-mode
  :ensure-system-package ccls
  :ensure-system-package (pyls . "python -m pip install pyls")
  :ensure-system-package rust-analyzer
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-completion-show-detail nil)
  (setq-default lsp-enable-indentation nil)
  (setq-default lsp-enable-on-type-formatting nil)
  :commands lsp
  :hook ((c-mode . lsp)
		 (c++-mode . lsp)
		 (python-mode . lsp)
		 (typescript-mode . lsp)
		 (rust-mode . lsp)))

;; TODO Dive deeper into LSP

(use-package lsp-ui
  :after lsp
  :init
  (setq lsp-ui-doc-delay 5)
  (add-hook 'flycheck-mode-hook 'lsp-ui-mode)
  :config 
  ;; HACK Hardcoded values are bad.
  (set-face-attribute 'lsp-ui-doc-background nil :background "#0b0f16"))


