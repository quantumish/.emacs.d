;; (general-define-key
;;  :keymaps 'prog-mode-map
;;  "C-c k" (lambda () (interactive)
;; 		   (split-window-right)
;; 		   (other-window 1)
;; 		  (call-process-shell-command
;; 		   (concat "kitty sh -c 'cd " (projectile-project-root) "; zsh'") nil 0)))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :init (counsel-projectile-mode))
(use-package treemacs-projectile)
(use-package flycheck-projectile)
