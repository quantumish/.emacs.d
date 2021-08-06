;; Various chores that need to be done before loading any config.
(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq custom-file (concat user-emacs-directory "/custom.el~"))
(require 'package)
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq package-archives '(("ELPA" . "https://tromey.com/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(defun load-module (module)
  "Load a user configuration module MODULE."
  (load-file (concat "~/.emacs.d/modules/" module ".el")))

(load-module "speed")

(require 'url-http)
(setq url-http-attempt-keepalives nil)
(setq package-check-signature nil)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(setq native-comp-deferred-compilation-deny-list nil)
(setq warning-suppress-log-types '((comp)))
(use-package no-littering :init (require 'no-littering))
(use-package general)

(load-module "system-pre")
(load-module "exwm")

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(if (string= (system-name) "qubit")
	(add-to-list 'exec-path "/home/quantumish/.local/bin"))

;; FIXME Get rid of header-line-spacious issues
(defun header-line-spacious ()
  (interactive)
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :height 200 :background "#0e121a"))

(defun set-header-line (height &optional dark)
  (setq header-line-format " ")
  (if dark
	  (set-face-attribute 'header-line nil :height height :background "#0b0f16")
	(set-face-attribute 'header-line nil :height height :background "#0e121a")))

(set-face-attribute 'default nil :family "IBM Plex Mono")

(defun determine-olivetti ()
  (interactive)
  (olivetti-set-width (- (window-total-width) 8)))

(if (eq system-type 'darwin)
	(progn
	  (setq mac-command-modifier 'meta)
	  (setq mac-option-modifier 'super)
	  (use-package ns-auto-titlebar
		:init (ns-auto-titlebar-mode))))

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
(load-module "help") ; 

(load-module "perspectives")
(load-module "movement-intraframe")
;; TODO: (load-module "movement-intrabuffer")
(load-module "selection")

(load-module "org")
(load-module "org-projects")
(load-module "org-aesthetic")
;; TODO: (load-module "org-gtd")

(load-module "lsp") ;
(load-module "company") ;
(load-module "compilation")
(load-module "documentation")
(load-module "git") ;
(load-module "snippets") ;
;; TODO: (load-module "debug")
(load-module "projectile")
(load-module "lint") ; 
(load-module "vc")

(load-module "c++")
;;(load-module "python")

(load-module "code-aesthetic")
(load-module "code-substitutions")

(load-module "writing")

;; TODO: (load-module "mu4e")
;; TODO: (load-module "rss")
;; (load-module "erc")

(load-module "scratch")

(if (eq system-type 'gnu/linux)
	;; FIXME: This needs to be loaded after EXWM and is prone to be breaking
	(add-hook 'exwm-init-hook (lambda () (load "exwm-outer-gaps")
					(exwm-outer-gaps-mode)
					(call-process-shell-command "bash ~/.config/polybar/launch.sh --docky" nil 0))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :hook (org-roam . org-roam-ui-mode))

(set-face-attribute 'font-lock-comment-face nil :italic t)

(require 'notifications)
(notifications-notify :title "Up and at 'em!"
					  :body (format "Loaded %d packages in %s with %d GCs."
		 (length package-activated-list)
		 (format "%.2f seconds"
				 (float-time
				  (time-subtract after-init-time before-init-time)))
		 gcs-done))
