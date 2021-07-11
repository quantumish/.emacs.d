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
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . (lambda () (set-window-fringes nil 15 0))))

(use-package consult-flycheck
  :bind
  ())

(use-package flycheck-clang-tidy
  :ensure-system-package clang-tidy
  :after flycheck
  :init
  (setq flycheck-clang-tidy-executable "/usr/bin/clang-tidy")
  (setq flycheck-clang-tidy-extra-options "-checks='*'")
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

(use-package flycheck-google-cpplint
  :ensure-system-package cpplint)
