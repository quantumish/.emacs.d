;;----------------------------------
;; Add some functionality to emacs
;;----------------------------------

(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 'flycheck)
(package-install 'lsp-mode)
(package-install 'lsp-ui)
(package-install 'markdown)
(package-install 'company)
(package-install 'company-box)
(package-install 'company-lsp)

(add-hook 'after-init-hook 'global-company-mode)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)

(global-flycheck-mode)
:lang
(c +lsp)
(c++ +lsp)
(python +lsp)

(provide 'functionality)
