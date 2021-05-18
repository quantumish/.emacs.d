;; Various chores that need to be done before loading any config.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'use-package)
(require 'package)
(setq package-archives '(("ELPA" . "https://tromey.com/elpa/")
						 ("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")))

(require 'url-http)
(setq url-http-attempt-keepalives nil)
(setq package-check-signature nil)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(use-package no-littering :init (require 'no-littering))
(use-package general)

(defun load-module (module)
  "Load a user configuration module MODULE."
  (load-file (concat "~/.emacs.d/modules/" module ".el")))

(load-module "speed")

(load-module "exwm")

(load-module "doom-ui")
(load-module "dashboard")
(load-module "exit-msg")

(load-module "vanilla")
(load-module "vanilla++")

(load-module "ivy")
(load-module "help")

(load-module "movement-intraframe")
;; TODO (load-module "movement-intrabuffer")

(load-module "selection")

(load-module "org")
(load-module "org-projects")
(load-module "org-aesthetic")

(load-module "lsp")
(load-module "company")
(load-module "compilation")
(load-module "documentation")
(load-module "git")

(load-module "code-aesthetic")
(load-module "code-substitutions")

(load-module "scratch")

;; FIXME This needs to be loaded after EXWM and is prone to be breaking
(add-hook 'exwm-init-hook (lambda () (load "exwm-outer-gaps")
							(exwm-outer-gaps-mode)
							(call-process-shell-command "bash ~/.config/polybar/launch.sh --docky" nil 0)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exwm-mff-mode t nil (exwm-mff))
 '(package-selected-packages
   '(gitignore-mode amx zygospore writeroom-mode writegood-mode which-key web-completion-data wc-mode wc-goal-mode vterm use-package typescript-mode treepy treemacs svg-tag-mode sudo-edit solaire-mode smooth-scrolling smooth-scroll smex smartparens shackle selected rainbow-mode quickrun projectile powerthesaurus popper pocket-reader parrot outshine origami org-superstar org-roam-server org-gcal org-fragtog org-bullets org-autolist olivetti no-littering multiple-cursors move-text mixed-pitch marginalia magit-todos lsp-ui lsp-ivy lsp-focus lively laas ivy-prescient ivy-posframe imenu-anywhere iedit hide-mode-line helpful helm goto-line-preview google-this git-gutter-fringe general gcmh format-all flyspell-correct-ivy flycheck exwm-mff exwm-float exwm-firefox-core exwm-edit expand-region ewal-doom-themes evil-collection esup embark dtrt-indent doom-modeline diredfl dired-rainbow dimmer diff-hl define-word dashboard crux counsel-dash company-wordfreq company-quickhelp-terminal company-prescient company-flx company-box cmake-mode centered-window centaur-tabs ccls beacon apiwrap all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-ibuffer all-the-icons-dired ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
