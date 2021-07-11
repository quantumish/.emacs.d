(general-define-key
 :keymaps 'prog-mode-map
 "C-c k" (lambda () (interactive)
		   (split-window-right)
		   (other-window 1)
		  (call-process-shell-command
		   (concat "kitty sh -c 'cd " (projectile-project-root) "; zsh'") nil 0)))

(use-package counsel-projectile)
(use-package treemacs-projectile)
(use-package flycheck-projectile)

;; (projectile-recentf)
;; (projectile-ibuffer)
;; (projectile-add-known-project)
;; (projectile-xc
