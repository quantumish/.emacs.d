(use-package compile
  :config
  (setq compilation-scroll-output t)
  (defun compile-project ()
	(interactive)
	; (shell-command "rm ./CMakeCache.txt && rm ./Makefile && rm -rf ./CMakeFiles")
	(let ((default-directory (projectile-project-root)))
	(call-interactively 'compile)))
  :bind (:map c++-mode-map
			  ("C-;" . compile-project)
			  ("C-c C-;" . recompile))
  :hook
  (compilation-mode . hide-mode-line-mode)
  (compilation-mode . header-line-spacious)
  (compilation-start . olivetti-mode)
  (compilation-start . determine-olivetti))

(general-def c++-mode-map
  "C-x n s" 'narrow-to-defun)
