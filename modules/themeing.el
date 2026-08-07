;; -*- lexical-binding: t; -*-
;;; Configuration for appearance related things.

(use-package doom-themes
  :config
  (set-face-attribute 'header-line nil :background (doom-color 'bg) :height 0.5)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package ewal)
(use-package ewal-doom-themes
 :init (load-theme 'ewal-doom-one t))

(use-package doom-modeline :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-encoding nil)
  :init (doom-modeline-mode))

(use-package hide-mode-line
  :init
  (if (featurep 'exwm)
      (add-hook 'exwm-mode-hook 'hide-mode-line-mode)))

(use-package solaire-mode
  :hook (prog-mode . solaire-mode))

(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :config
  (setq dired-listing-switches "-alh")
  :hook (dired-mode . diredfl-mode))
