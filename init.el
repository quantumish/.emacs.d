;; Various chores that need to be done before loading any config.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file (concat user-emacs-directory "/custom.el~")) 
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
;; TODO: (load-module "quantum-ui")
(load-module "dashboard")
(load-module "exit-msg")
;; TODO: (load-module "popup")
;; TODO: (load-module "centered")

;; TODO: (load-module "evil")

(load-module "vanilla")
(load-module "vanilla++")

(load-module "ivy")
(load-module "help")

(load-module "movement-intraframe")
;; TODO: (load-module "movement-intrabuffer")
(load-module "selection")

(load-module "org")
(load-module "org-projects")
(load-module "org-aesthetic")
;; TODO: (load-module "org-gtd")

(load-module "lsp")
(load-module "company")
(load-module "compilation")
(load-module "documentation")
(load-module "git")
;; TODO: (load-module "snippets")
;; TODO: (load-module "debug")

;; TODO: (load-module "c++")
;; TODO: (load-module "python")

(load-module "code-aesthetic")
(load-module "code-substitutions")

;; TODO: (load-module "writing")

;; NOTE: This is a pipe dream. 
;; TODO: (load-module "mu4e")

(load-module "scratch")

;; FIXME: This needs to be loaded after EXWM and is prone to be breaking
(add-hook 'exwm-init-hook (lambda () (load "exwm-outer-gaps")
							(exwm-outer-gaps-mode)
							(call-process-shell-command "bash ~/.config/polybar/launch.sh --docky" nil 0)))
