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
(use-package system-packages
  :init
  (add-to-list 'system-packages-supported-package-managers
			   '(yay .
					 ((default-sudo . nil)
					  (install . "yay -S")
					  (uninstall . "yay -Rs")
					  (log . "cat /var/log/pacman.log")
					  (change-log . "yay -Qc")
					  (get-info . "yay -Qi")
					  (get-info-remote . "yay -Si")
					  (list-files-provided-by . "yay -Ql")
					  (owning-file . "yay -Qo")
					  (verify-all-dependencies . "yay -Dk")
					  (remove-orphaned . "yay -Rsn $(pacman -Qtdq)")
					  (list-installed-packages . "yay -Qe")
					  (list-installed-packages-all . "yay -Q")
					  (noconfirm . "--noconfirm"))))
  (setq system-packages-noconfirm t)
  (setq system-packages-package-manager 'yay)
  (setq system-packages-use-sudo nil))
(use-package use-package-ensure-system-package)

(if (eq system-type 'gnu/linux)
	(progn
	  (use-package exwm
		:ensure-system-package (xbindkeys xcape dunst flameshot unclutter polybar))
	  (call-process-shell-command "xmodmap ~/.config/X/.xmodmap" nil 0)
	  (call-process-shell-command "xbindkeys" nil 0)
	  (call-process-shell-command "sh ~/.config/X/xcape.sh" nil 0)
	  (call-process-shell-command "dunst &" nil 0)
	  (call-process-shell-command "sh ~/.config/dunst/reload_dunst.sh" nil 0)
	  (call-process-shell-command "unclutter &" nil 0)
	  (call-process-shell-command "flameshot &" nil 0)
	  ;; TODO Randomly decides to reinstall things sometimes
	  ;; (use-package exwm
	  ;; 	:ensure-system-package (rustup cmake python38 python38-pip)
	  ;; 	:ensure-system-package (syncthing activitywatch)
	  ;; 	:ensure-system-package (firefox kitty discord spotify steam dropbox zathura pavucontrol) ; intel-vtune-profiler
	  ;; 	:ensure-system-package (lsd rm-improved fd bat hyperfine gotop unzip tig) ; ripgrep
	  ;; 	:ensure-system-package (slock xclip rofi mpd mpv texlive-most) ; pandoc-bin
	  ;; 	:ensure-system-package (neofetch unimatrix pipes.sh))
	  ))
 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(if (eq system-type 'gnu/linux)
	(add-to-list 'exec-path "/home/quantumish/.local/bin"))

;; FIXME Get rid of header-line-spacious issues
(defun header-line-spacious ()
  (interactive)
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :height 200 :background "#0e121a"))

;; (defun header-line-spacious-dark ()
;;   (interactive)

(defun set-header-line (height &optional dark)
  (setq header-line-format " ")
  (if dark
	  (set-face-attribute 'header-line nil :height height :background "#0b0f16")
	(set-face-attribute 'header-line nil :height height :background "#0e121a")))

(defun determine-olivetti ()
  (interactive)
  (olivetti-set-width (- (window-total-width) 8)))

(if (eq system-type 'darwin)
	(progn
	  (setq mac-command-modifier 'meta)
	  (setq mac-option-modifier 'super)
	  (use-package ns-auto-titlebar
		:init (ns-auto-titlebar-mode))))

(defun load-module (module)
  "Load a user configuration module MODULE."
  (load-file (concat "~/.emacs.d/modules/" module ".el")))

(load-module "speed")

(if (eq system-type 'gnu/linux)
	(load-module "exwm"))
;; TODO: (load-module "environ")

(use-package projectile)

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

(load-module "perspectives")
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
(load-module "snippets")
;; TODO: (load-module "debug")
(load-module "projectile")
(load-module "lint")
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

(message "Emacs loaded (with %d packages) in %s with %d garbage collections."
		 (length package-activated-list)
		 (format "%.2f seconds"
				 (float-time
				  (time-subtract after-init-time before-init-time)))
		 gcs-done)
