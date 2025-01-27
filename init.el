;; -*- lexical-binding: t; -*-
;; Emacs config: rebooted

;; Hopefully built to last!

;; Setup

;: IF YOU ARE DEBUGGING MYSTERIOUS CRASHES MAYBE TRY FC-CACHE -VF OK?

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
										(or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
										"--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
		  (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; (setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package org)


;; Better key bind management package.
(use-package general :ensure (:wait t) :demand t)

;;; Defaults

(defun my/simple-setup ()
  ;; Turn off random GUI cruft
  (setq ring-bell-function #'ignore)
  
  ;; Not personally a fan of customize.el
  (setq custom-file "/dev/null")
  (setq package-selected-packages "/dev/null/")

  ;; These keybinds suspend Emacs (in order to mimic terminal behavior).
  ;; This has *only* caused me trouble in GUI Emacs.
  (when (display-graphic-p)
    (global-unset-key (kbd "C-z"))
    (global-unset-key (kbd "C-x C-z")))

  ;; Stop making backup files everywhere, put them all in one place!
  (make-directory "~/.saves/" t)
  (make-directory "~/.autosaves/" t)
  (setq auto-save-file-name-transforms '((".*" "~/.autosaves/" t)))
  (setq backup-directory-alist `(("." . "~/.saves")))
  (setq backup-by-copying t)
  
  ;; Stop Emacs from bothering you about disabled commands.
  (setq disabled-command-function nil)

  ;; Stop Emacs from trying to use dialog boxes.
  (setq use-dialog-box nil)

  ;; Prefer y/n over yes/no.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Stuff is often here too.
  (add-to-list 'exec-path "~/.local/bin")

  ;; Visual preference.
  (global-visual-line-mode)

  ;; PgUp and PgDown are scarily close to arrow keys on my laptop.
  (when (string= (system-name) "qpad")
    (unbind-key "C-<prior>")
    (unbind-key "C-<next>")))

										; (add-hook 'exwm-init-hook 'my/simple-setup)

;; EXWM

(use-package exwm
  :config
  (setq exwm-workspace-number 2)
  (setq exwm-input-global-keys  `(([?\s-r] . exwm-reset)))
  (setq exwm-input-simulation-keys
        '(([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-s] . ?\C-f)
 		  ([?\M-w] . ?\C-c)
 		  ([?\C-y] . ?\C-v)
          ([?\C-x?\C-s] . ?\C-s)
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
		  ([?\C-k] . [S-end delete])))

  ;; (exwm-input-set-local-simulation-keys '(([?\C-x?\C-s] . ?\C-f)))
  
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-workspace-show-all-buffers t)
  ;; (setq exwm-workspace-minibuffer-position 'bottom)

  (defun b3n-exwm-set-buffer-name ()
    (if (and (stringp exwm-title) (= (length (s-split " — " exwm-title)) 3))
        (cl-destructuring-bind (title url _) (s-split " — " exwm-title)
          (setq-local buffer-file-name url)
          (setq-local exwm-title
    				  (concat exwm-class-name "<"
    						  (if (<= (length title) 50)
    							  title (concat (substring title 0 50) "…"))
    						  ">")))
      (setq-local exwm-title
    			  (concat
                   exwm-class-name
                   "<"
                   (if (<= (length exwm-title) 50)
                       exwm-title
    				 (concat (substring exwm-title 0 50) "…"))
                   ">")))

    (exwm-workspace-rename-buffer exwm-title))

  (defun opposite-other-window ()
    "Cycle buffers in the opposite direction."
    (interactive)
    (other-window -1))
  
  (defun opposite-other-frame ()
    "Cycle frames in the opposite direction."
    (interactive)
    (other-frame -1))
  
  (require 'exwm-randr)				;
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-1"))
  (exwm-randr-enable)
  
  :general
  (:keymaps '(override-global-map exwm-mode-map)
			"M-k" 'other-window
			"M-j" 'opposite-other-window
			"C-M-j" 'opposite-other-frame
			"C-M-k" 'other-frame)

  :hook
  ((exwm-update-class . b3n-exwm-set-buffer-name)
   (exwm-update-title . b3n-exwm-set-buffer-name))
  :init (exwm-enable))

(use-package prodigy :demand t
  :config
  (setq prodigy-kill-process-buffer-on-stop t)

  (defun my/prodigy-define-services ()
  	(prodigy-define-service
	  :name "Browse-url HTTP server"
	  :url "http://localhost:8000"
	  :command "python" :args '("silly.py")
	  :cwd "/home/quantumish" :tags '(hack)
	  :auto-start t)
	(prodigy-define-service
	  :name "Dunst" :command "dunst"
	  :tags '(x11) :auto-start t)
	(prodigy-define-service
	  :name "Pipewire" :command "gentoo-pipewire-launcher"
	  :tags '(audio) :auto-start t)
	(prodigy-define-service
	  :name "earlyOOM" :command "earlyoom" :args '("-r" "0")
	  :tags '(stability) :auto-start t)
	(prodigy-define-service
	  :name "Unclutter" :command "unclutter"
	  :tags '(x11) :auto-start t)
	(prodigy-define-service
	  :name "Xbindkeys" :command "xbindkeys"
	  :args '("-n") :tags '(x11) :auto-start t)
	(prodigy-define-service
	  :name "Picom" :command "picom"
	  :tags '(x11)  :auto-start t)
	(prodigy-define-service
	  :name "Syncthing" :command "syncthing"
	  :args '("-no-browser" "-gui-address=http://127.0.0.1:8384")
	  :url "http://localhost:8384"
	  :tags '(sync) :auto-start t)

	(dolist (x (seq-filter #'stringp exwm-randr-workspace-monitor-plist))
	  (prodigy-define-service
		:name (format "Polybar (%s)" x) :command "polybar"
		:args (list "-q" "main" "-c" (concat (getenv "HOME") "/.config/polybar/shades/config.ini"))
		:env (list (list "MONITOR" x))
		:tags '(theme) :auto-start t)))

  (defun prodigy-autostart ()
	"Start all unstarted services with `:auto-start' set to non-nil."
	(interactive)
	(prodigy-with-refresh
	 (--each prodigy-services
       (when (and (plist-get it :auto-start)
       	          (not (prodigy-service-started-p it)))
		 (prodigy-start-service it)))))
  
  (defun prodigy-killall ()
	(interactive)
	(prodigy-with-refresh
	 (--each prodigy-services (prodigy-stop-service it))))

  :hook ((exwm-init . (lambda () (my/prodigy-define-services) (prodigy-autostart)))
		 (kill-emacs . prodigy-killall)))

(use-package emacs :ensure nil
  :config
  (defun show-welcome-buffer ()
	"Show *Welcome* buffer."
	(with-current-buffer (get-buffer-create "*Splash*")
      (setq truncate-lines t)
      (let* ((buffer-read-only)
			 (image-path "~/Downloads/Group 1.png")
			 (image (create-image image-path))
			 (size (image-size image))
			 (height (cdr size))
			 (width (car size))
			 (top-margin (floor (/ (- 84 height) 2)))
			 (left-margin (floor (/ (- 267 width) 2))))
		(erase-buffer)
		(setq mode-line-format nil)
		(goto-char (point-min))
		(insert (make-string top-margin ?\n ))
		(insert (make-string left-margin ?\ ))
		(insert-image image))
      (setq cursor-type nil)
      (read-only-mode +1)
      (switch-to-buffer (current-buffer))))
  :hook
  (exwm-init . show-welcome-buffer))

(use-package exwm-outer-gaps
  :ensure (:host github :repo "walseb/exwm-outer-gaps")
  :config
  (defun exwm-outer-gaps-redraw ()
    "exwm-outer gaps sometimes has artifacts in the gap area.
     Quickly toggling the mode on and off works forces a redraw of
     the gaps and gets rid of them."
    (interactive)
    (exwm-outer-gaps-mode))
  (setq exwm-outer-gaps-width [25 25 25 0])
  :hook (exwm-init . exwm-outer-gaps-mode))

(use-package exwm-mff
  :hook (exwm-init . exwm-mff-mode))

;;; Completion

(use-package vertico
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
		 (exwm-init . vertico-mode)))		 

(use-package emacs :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :general (:keymaps 'minibuffer-local-map
					 "M-A" 'marginalia-cycle)
  :hook (exwm-init . marginalia-mode))

(use-package embark
  :general
  ("C-." 'embark-act)
  ("M-." 'embark-dwim)
  ("C-h B" 'embark-bindings)

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Example configuration for Consult
(use-package consult
  :general ;; TODO there's more but im lazy
  ("C-c M-x" 'consult-mode-command)
  ("C-c h" 'consult-history)
  ("C-c m" 'consult-man)
  ("C-c i" 'consult-info)
  ("C-x M-:" 'consult-complex-command)
  ("C-x b" 'consult-buffer)
  ("C-x p b" 'consult-project-buffer)
  ("M-g g" 'consult-goto-line)             ;; orig'goto-line
  ("M-g M-g" 'consult-goto-line)           ;; orig'goto-line
  ("M-g o" 'consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" 'consult-mark)
  ("M-g k" 'consult-global-mark)
  ("M-g i" 'consult-imenu)
  ("M-g I" 'consult-imenu-multi)
  ("M-s d" 'consult-fd)
  ("M-s r" 'consult-ripgrep)
  ("C-s" 'consult-line)
  ("M-s L" 'consult-line-multi)
  ("M-s k" 'consult-keep-lines)
  ("M-s u" 'consult-focus-lines)
  (:keymaps 'minibuffer-local-map
			"M-s" 'consult-history
			"M-r" 'consult-history)
  
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-buffer consult-line-multi consult-global-mark
   consult-project-buffer consult-imenu-multi
   :preview-key nil
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "C-+"))

(use-package embark-consult 
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Help

(use-package helpful
  
  :init
  ;; Advise describe-style functions so that Helpful appears no matter what
  (advice-add 'describe-function :override #'helpful-function)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-command :override #'helpful-callable)
  (advice-add 'describe-key :override #'helpful-key)
  (advice-add 'describe-symbol :override #'helpful-symbol)

  :general
  ("C-h f" 'helpful-function)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-c C-d" 'helpful-at-point)
  ("C-h F" 'helpful-function)
  ("C-h C" 'helpful-command))

(use-package which-key
  :hook (exwm-init . which-key-mode))

;;; handy vanilla stuff

(use-package crux
  :general
  ("C-a" 'crux-move-beginning-of-line)
  ("C-x 4 t" 'crux-transpose-windows)
  ("C-x K" 'crux-kill-other-buffers)
  ("C-k" 'crux-smart-kill-line)
  ("C-<tab>" 'crux-indent-defun)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package rainbow-mode)

(use-package sudo-edit)

(use-package drag-stuff
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package emacs :ensure nil
  :general
  ("<pinch>" 'ignore)
  ("<C-wheel-up>" 'ignore)
  ("<C-wheel-down>" 'ignore))

;; terminals whee

(use-package vterm
  :hook (vterm-mode . rename-uniquely))

(use-package eat :ensure 
  (:host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
				 "*.ti" ("terminfo/e" "terminfo/e/*")
				 ("terminfo/65" "terminfo/65/*")
				 ("integration" "integration/*")
				 (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (defun ncmpcpp ()
    (interactive)
    (call-process-shell-command "kitty ncmpcpp" nil 0)))


;; theming

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))


(use-package ewal)
(use-package ewal-doom-themes
  :hook (exwm-init . (lambda () (load-theme 'ewal-doom-one t))))

(use-package doom-modeline :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-encoding nil)
  :hook (exwm-init . doom-modeline-mode))

(use-package nerd-icons-completion
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package solaire-mode
  :hook (prog-mode . solaire-mode))

(use-package diredfl
  :config
  (setq dired-listing-switches "-alh")
  :hook (dired-mode . diredfl-mode))

;;; org mode

(use-package org :ensure nil
  :config
  (set-face-attribute 'org-document-title nil
                      :height 2.0 :weight 'bold)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-emphasized-text t)
  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis " ")
  (setq org-hide-leading-stars t)
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-highlight-latex-and-related '(native))  
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "NOPE(n)")))  
  :hook (org-mode . org-indent-mode))

;; (use-package org-variable-pitch
;;   :init (org-variable-pitch-setup))

;; Auto-toggle emphasis
(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

;; Natural bulleted lists
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/sync2/truenotes"))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)  
  :general
  ("C-c n l" 'org-roam-buffer-toggle)
  ("C-c n f" 'org-roam-node-find)
  ("C-c n g" 'org-roam-graph)
  ("C-c n i" 'org-roam-node-insert)
  ("C-c n c" 'org-roam-capture)
  ;; Dailies
  ("C-c n j" 'org-roam-dailies-capture-today))

(use-package org-download
  :config
  (setq org-download-method 'attach))

(use-package org-roam-ui
  :ensure
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; (use-package org-roam-ui
;;   :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;   :after org-roam
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;;  ;; :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))


(use-package haskell-mode)
(use-package go-mode)
(use-package sml-mode)
(use-package rust-mode)

(use-package emacs :ensure nil
  :custom (treesit-font-lock-level 3)
  :config
  (setq major-mode-remap-alist
		'((bash-mode . bash-ts-mode)   
		  (json-mode . json-ts-mode)
		  (c-mode . c-ts-mode)
		  (python-mode . python-ts-mode)
		  (c++-mode . c++-ts-mode)
		  (rust-mode . rust-ts-mode))))

;; porcelain esque things

(use-package deadgrep)
(use-package transient)
(use-package magit)

;; TODO mess with this
;; (use-package grugru
;;   :general ("C-;" 'grugru) 
;;   :config (grugru-default-setup))

;;; pdfs

(use-package hide-mode-line
  :hook (exwm-mode . hide-mode-line-mode))

(use-package pdf-tools
  
  :init (pdf-loader-install)
  :config
  (defun my/pdf-fit ()
    "Calculates the difference between the pdf image and frame width
     before resizing the frame by the difference"
    (interactive)
    (let ((W (- (car (pdf-view-image-size)) (window-pixel-width))))
      (window-resize (get-buffer-window) (+ 10 W) t nil t)
      (pdf-view-fit-height-to-window)))

  :hook
  ((pdf-view-mode . pdf-view-midnight-minor-mode)
   (pdf-view-mode . hide-mode-line-mode)
   (pdf-view-after-change-page . my/pdf-fit)))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (defun slime-eval-last-expression-eros ()
    (interactive)
    (cl-destructuring-bind (output value)
        (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
								 :where (point)
								 :duration eros-eval-result-duration)))

  (use-package coalton-mode
    :ensure (:host github :repo "coalton-lang/coalton-mode"))
  
  ;; (add-hook 'lisp-mode-hook 'slime)
  ;; (add-hook 'lisp-mode-hook 'slime-mode)
  ;; (general-def :keymaps 'slime-mode-map
  ;;   "C-x C-e" 'cmake-ide-compile))

  :bind (:map slime-mode-map
              ("C-x C-e" . slime-eval-last-expression-eros))
  :hook ((lisp-mode . slime)
         (lisp-mode . slime-mode)))


(use-package org :ensure nil
  :config
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-latex-default-packages-alist
		'(("AUTO" "inputenc" t ("pdflatex")) 
          ("T1" "fontenc" t ("pdflatex"))
          (#1="" "graphicx" t)
          (#1# "longtable" nil)
          (#1# "wrapfig" nil)
          (#1# "rotating" nil)
          ("normalem" "ulem" t)
          (#1# "amsmath" t)
          (#1# "amssymb" t)
          (#1# "capt-of" nil)
          ("colorlinks" "hyperref" nil)
          (#1# "geometry" nil)
          ("parfill" "parskip" nil))))

;; Kind of cool feature. TODO experiment with me.
;; (use-package engrave-faces
;;   :config
;;   (setq org-latex-listings 'engraved)
;;   (setq org-latex-engraved-theme 'doom-one))

;; (use-package htmlize)

;; (use-package org-special-block-extras
;;   :hook (org-mode . org-special-block-extras-mode)
;;   :config
;;   (org-defblock fancyquote (author "Unknown")
;;    (pcase backend
;;      (`latex (format "\\epigraph{%s}{\\emph{%s}}\\vspace{10pt}" contents author))
;;      (_ "")) ;; TODO handle HTML cases
;;    ))

(use-package lsp-mode
										; :ensure-system-package ccls
										; :ensure-system-package (pyls . "python -m pip install pyls")
										; :ensure-system-package rust-analyzerf
  :config
  ;; Disable annoying headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Don't show unneeded function info in completions
  (setq lsp-completion-show-detail nil)
  ;; Disable annoying autoformatting!
  (setq-default lsp-enable-indentation nil)
  (setq-default lsp-enable-on-type-formatting nil)
  :commands lsp
  ;; Add languages of your choice!
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp)
		 (python-ts-mode . lsp)
         (typescript-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . (lambda () (lsp-lens-mode 0)))))

(use-package lsp-ui
  :after lsp
  :config
  (setq lsp-ui-doc-delay 5)
  (eval `(set-face-attribute 'lsp-ui-doc-background nil :background ,(doom-darken 'bg .2)))
  :hook ((flycheck-mode . lsp-ui-mode) ;; HACK
		 (lsp-mode . lsp-ui-mode)))

;;;; Company
(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (lsp-mode . company-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package consult-flycheck)

;; (use-package idris2-mode
;;   :ensure (:host github :repo "idris-community/idris2-mode"))

(use-package xenops
  :hook (org-mode . xenops-mode)
  :config (defun xenops-handle-paste ()))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
		 (org-mode . yas-minor-mode)))

(use-package aas
  :hook (LaTeX-mode . ass-activate-for-major-mode)
  :hook (org-mode . ass-activate-for-major-mode)
  :hook (c-mode . ass-activate-for-major-mode)
  :hook (c++-mode . ass-activate-for-major-mode)
  :config
  (aas-set-snippets 'c-mode
                    "u64" "uint64_t"
                    "u32" "uint32_t"
                    "u16" "uint16_t"
                    "u8" "uint8_t"
                    "i64" "int64_t"
                    "i32" "int32_t"
                    "i16" "int16_t"
                    "i8" "int8_t"
                    "sz" "size_t")
  (aas-set-snippets 'c++-mode
                    "mxf" "Eigen::MatrixXf"
                    "mxd" "Eigen::MatrixXd"
                    "v2f" "Eigen::Vector2f"
                    "v2d" "Eigen::Vector2d"
                    "v2i" "Eigen::Vector2i"
                    "v3f" "Eigen::Vector3f"
                    "v3d" "Eigen::Vector3d"
                    "v3i" "Eigen::Vector3i")
  (aas-set-snippets 'org-mode
            "w/" "with"
            "--" "—"))
(use-package laas
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    "mk" (lambda () (interactive)
                           (yas-expand-snippet "\\\\($1\\\\)$0"))                      
                    "afsoc" "assume for the sake of contradiction"
                    "Afsoc" "Assume for the sake of contradiction"
                    :cond #'texmathp ; expand only while in math
                    "tt" (lambda () (interactive)
                           (yas-expand-snippet "\\text{$1}$0"))
                    "dd" (lambda () (interactive)
                           (yas-expand-snippet "\\dd{$1}$0"))                      
                    "'-" "\\setminus"
                    "reals" "\\mathbb{R}"
                    "ints" "\\mathbb{Z}"
                    "nats" "\\mathbb{N}"
                    "pi" "\\pi"
                    "bff" (lambda () (interactive)
                            (yas-expand-snippet "\\mathbf{$1}$0"))
                    "ll" "\\left"
                    "rr" "\\right"
                    "pm" (lambda () (interactive)
                           (yas-expand-snippet "\\begin{pmatrix} $1 \\end{pmatrix} $0"))
                    "sm" (lambda () (interactive)
                           (yas-expand-snippet "\\left(\\begin{smallmatrix} $1 \\end{smallmatrix}\\right) $0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
                    ))

(use-package olivetti
  :config (setq-default olivetti-width 120)
  :hook (org-mode . olivetti-mode))

(c-set-offset 'innamespace 0)

(use-package dmenu
  :general (:keymaps '(override-global-map exwm-mode-map)
					 "M-p" 'dmenu))

(defun find-and-switch-to-app (app-name)
  "Looks for a buffer that probably corresponds to the window of an app."
  (let* ((predicate
		  (lambda (b)
			(and (string-match-p (regexp-quote (format "%s |" app-name)) (buffer-name b))
				 (eq (buffer-local-value 'major-mode b) 'exwm-mode))))
		 (cands (seq-filter predicate (buffer-list))))
    (if (car cands)
		(or (switch-to-buffer (car cands)) t))))

(defun open-app (app-name)
  "Switches to any open EXWM buffer of app or opens app."
  (if (not (find-and-switch-to-app "discord"))
      (call-process-shell-command "firefox-bin --new-window discord.com/app" nil 0)))    

(defun my/run-shell-cmd (cmd)
  call-process-shell-command "firefox-bin --new-window" nil 0)

(call-process-shell-command "amixer set Master 5%-" nil 0)

(defmacro named-cmd (name cmd)
  `(defun ,name ()
	 (interactive)
	 (call-process-shell-command ,cmd nil 0)))

(named-cmd firefox-new-tab "firefox-bin --new-window")
(named-cmd lock-screen "betterlockscreen -l blur")
(named-cmd decrease-volume-5 "amixer set Master 5%-")
(named-cmd increase-volume-5 "amixer set Master 5%+")
(named-cmd toggle-volume "amixer set Master toggle")
(named-cmd powermenu "~/.config/rofi/applets/bin/powermenu.sh")
(named-cmd flameshot "flameshot gui")

(general-def
  :keymaps '(override-global-map exwm-mode-map)
  "M-d" (lambda () (interactive) (open-app "discord"))
  "M-f" #'firefox-new-tab
  "M-l" #'lock-screen
  "<XF86AudioLowerVolume>" #'decrease-volume-5
  "<XF86AudioRaiseVolume>" #'increase-volume-5
  "<XF86AudioMute>" #'toggle-volume
  "M-<delete>" #'powermenu
  "M-s M-s" #'flameshot)

(set-display-table-slot standard-display-table 0 ?\ )

;; (use-package hl-todo
;;   :config
;;   (setq hl-todo-keyword-faces
;;       `(("TODO"   . ,(doom-color 'green))
;;         ("FIXME"  . ,(doom-color 'red))
;;         ("DEBUG"  . ,(doom-color 'magenta))
;;         ("HACK"   . ,(doom-color 'violet))
;;         ("NOTE"   . ,(doom-color 'cyan))))
;;   :hook (prog-mode . hl-todo-mode))

(setq-default tab-width 4)
(add-to-list 'exec-path (format "%s/go/bin" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/.cargo/bin" (getenv "HOME")))
(add-to-list 'exec-path "/usr/local/go/bin")

(use-package consult-dash
  :config
  (setq dash-docs-browser-func
		(lambda (x)
		  (call-process-shell-command 
		   (format "firefox-bin --new-window localhost:8000%s"
				   (substring x (length "file:///")))
		   nil 0))))

(use-package consult-todo) 

(use-package elfeed
  :config
  (setq elfeed-feeds
      '(("https://www.birdsoutsidemywindow.org/feed/" nature)
		("https://siftrss.com/f/XnAo0e0AMb6" cs) ;; https://abhinavsarkar.net/
		("https://buttondown.com/hillelwayne/rss" cs)
		("https://www.filfre.net/feed/" games)
		("https://ciechanow.ski/atom.xml")
		("https://mazzo.li/rss.xml" cs)
		("https://reasonablypolymorphic.com/atom.xml" cs)
		("https://www.righto.com/feeds/posts/default" hardware)		
		("https://siftrss.com/f/aorvoWYqnm" cs links)))	;; https://notes.abhinavsarkar.net/
  :general
  ("C-c e" 'elfeed))



(add-hook
 'dired-mode-hook
 (lambda ()
   (call-process-shell-command	
	(format "zoxide add %s"  dired-directory) nil 0)))

(add-hook
 'find-file-hook
 (lambda ()
   (call-process-shell-command
	(format "zoxide add %s"  (file-name-directory buffer-file-name))
	nil 0)))

(defun find-file-with-zoxide ()
  (interactive)
  (let ((target (consult--read
				 (consult--async-command (lambda (x) (list "zoxide" "query" "-l" x))
				   (consult--async-map #'identity))
				 :prompt "Zoxide  "
				 :require-match nil
				 :lookup #'consult--lookup-member
				 :initial (consult--async-split-initial "")
				 :require-match t
				 :category 'file
				 :sort nil)))
	(if target
		(let ((default-directory (concat target "/")))
		  (call-interactively 'find-file))
	  (call-interactively 'find-file))))

(use-package dash)

(set-face-attribute 'variable-pitch nil :font "IBM Plex Sans")
;; (set-face-attribute 'default nil :font "uw ttyp0")

(add-hook 'exwm-init-hook
		  (lambda ()
			(load (expand-file-name "~/.emacs.d/hacks/gentoo.el") nil t t)))

(use-package consult-dir
  :config
  ;; A function that returns a list of directories
  (defun consult-dir--zoxide-dirs ()
	"Return list of fasd dirs."
	(mapcar (lambda (x) (replace-regexp-in-string  "^/home/quantumish" "~" x))
			(split-string (shell-command-to-string "zoxide query -l") "\n" t)))

  ;; A consult source that calls this function
  (defvar consult-dir--source-zoxide
	`(:name "Zoxide directories"
			:narrow   ?f
			:category file
			:face     consult-file
			:history  file-name-history
			:enabled  ,(lambda () (executable-find "zoxide"))
			:items    ,#'consult-dir--zoxide-dirs)
	"Fasd directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

;; (scroll-on-jump-advice-add forward-paragraph)
;; (scroll-on-jump-advice-add backward-paragraph)

(defun my/hacky-padding (w)
  (with-current-buffer (window-buffer w)
	(unless (or (minibufferp (window-buffer w)) (derived-mode-p 'exwm-mode) )
	  (setq-local header-line-format " ")
	  (if (derived-mode-p 'vterm-mode)
		  (face-remap-add-relative 'header-line '(:height 0.5)
								   (face-remap-add-relative 'header-line '(:height 2.0))))
	  (set-window-margins w 2 2))))

(add-hook 'window-state-change-hook (lambda () (walk-windows 'my/hacky-padding nil nil)))

(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (set-face-attribute 'flycheck-error nil  :underline `(:color ,(doom-color 'red)))
  (set-face-attribute 'flycheck-warning nil  :underline `(:color ,(doom-color 'orange)))
  (set-face-attribute 'flycheck-info nil  :underline `(:color ,(doom-color 'yellow))))

(use-package ready-player :ensure t
  :init (ready-player-mode)
  :general
  ("<XF86AudioNext>" 'ready-player-next)
  ("<XF86AudioPrev>" 'ready-player-previous)
  ("<XF86AudioPlay>" 'ready-player-toggle-play-stop)
  ("<XF86AudioPause>" 'ready-player-toggle-play-stop))

(defun my/sml-run (&optional compile)
  (interactive)
  (save-buffer)
  (let* ((dname (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))))
         (pname (concat dname  ": smlnj"))
         (buf (get-buffer (concat "*" pname "*"))))
    (when buf
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))
    (save-current-buffer      
      (pop-to-buffer (apply 'make-comint pname "sml" nil '(buffer-file-name)))  
      (inferior-sml-mode)
      (goto-char (point-max))
      (shrink-window (max 0 (- (window-total-width) 80)) t))
    (when compile (other-window -1))))

(use-package sml-mode
  :config
  (setq inferior-sml-font-lock-keywords sml-font-lock-keywords)
  (add-to-list 'auto-mode-alist '("\\.fun\\'" . sml-mode))
  (bind-key "C-c C-c" (lambda () (interactive) (my/sml-run t)) sml-mode-map))



(add-hook 'eshell-mode-hook 'hide-mode-line-mode)

(setq eshell-banner-message "")

(abbreviate-file-name "//home/quantumish")


(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name (replace-regexp-in-string ".*//" "/"  current-path))
      (string-remove-prefix (file-name-directory git-output) current-path))))


;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.

(defun dw/eshell-prompt ()
  (concat
   "\n"
   (propertize "davfrei" 'face `(:foreground ,(doom-color 'orange)) 'read-only t)
   (propertize " " 'face `(:foreground ,(doom-color 'fg)) 'read-only t)
   (propertize (dw/get-prompt-path) 'face `(:foreground ,(doom-color 'orange)) 'read-only t)
   (propertize " · " 'face `(:foreground ,(doom-color 'fg)) 'read-only t)
   (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground ,(doom-color 'cyan)) 'read-only t)
   (if (= (user-uid) 0)
       (propertize "\n#" 'face `(:foreground "red2") 'read-only t)
     (propertize "\nλ" 'face `(:foreground ,(doom-color 'blue)) 'read-only t))
   (propertize " " 'face `(:foreground ,(doom-color 'fg)))
   ))

(use-package xterm-color :ensure t)
(defun dw/eshell-configure ()
  (push 'eshell-tramp eshell-modules-list)
  (push 'eshell-smart eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(use-package eshell :ensure nil
  :hook (eshell-first-time-mode . dw/eshell-configure)
  :init
  (setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))

(use-package eshell-z
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
         (eshell-z-change-dir .  (lambda () (eshell/pushd (eshell/pwd))))))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq eshell-prompt-function 'dw/eshell-prompt)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-toggle
  :straight (eshell-toggle :type git :host github :repo "4DA/eshell-toggle")
  :init
  (setq eshell-toggle-size-fraction 4)
  (setq eshell-toggle-use-projectile-root t)
  (setq eshell-toggle-run-command nil))

(use-package eshell-up) ;; TODO eshell-up

(setq org-agenda-files '("~/sync2/spring2025.org" "~/sync2/life.org"))

(defun org-agenda-skip-if-unactionable ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
          (now (time-to-seconds (current-time)))
          (scheduled-seconds
            (time-to-seconds
              (org-time-string-to-time
                (or (org-entry-get nil "DEFERRED") now)))))
       (and scheduled-seconds
            (>= scheduled-seconds now)
            subtree-end))))

(org-add-agenda-custom-command
 '("x" agenda "ACTIONABLE"
   ((org-agenda-skip-function 'org-agenda-skip-if-unactionable)
    (org-agenda-overriding-header "Actionable entries: "))))
x

(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0))  


(setq large-file-warning-threshold (* 10 large-file-warning-threshold))
