;;; Early Init
;;;; Speed
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
	  gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
	(setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  "Disable garbage collection at init."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore garbage collection after init."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
		gcmh-high-cons-threshold (* 16 1024 1024) ))

(setq package-enable-at-startup nil		; don't auto-initialize!
	  ;; don't add that `custom-set-variables' block to my init.el!
	  package--init-file-ensured t)

(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

;;;; Packages and Elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'package)
(setq package-archives '(("ELPA" . "https://tromey.com/elpa/")
						 ("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")))
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;	  (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;	(with-current-buffer
;;		(url-retrieve-synchronously
;;		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;		 'silent 'inhibit-cookies)
;;	  (goto-char (point-max))
;;	  (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
(setq disabled-command-function nil)

;(require 'url-http)
(setq url-http-attempt-keepalives nil)
(setq package-check-signature nil)
;(require 'use-package)
;(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;;;; native-comp
(setq package-native-compile t)
(setq comp-deferred-compilation t)
;; (native-compile-async "~/.emacs.d/elpa" 'recursively)

;;;; General
(use-package general)

;;;; EXWM
(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "HDMI-1" 1 "DP-3" 2 "HDMI-1" 3 "DP-3"))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
		(start-process-shell-command
		 "xrandr" nil "xrandr --output DP-3 --rotate left --left-of HDMI-1")
		(call-process-shell-command "feh --bg-fill ~/.config/wallpapers/firewatch-galaxy.jpg" nil 0)))
(exwm-randr-enable)

(defun exwm-workspace-next ()
  (interactive)
  (if (< exwm-workspace-current-index (- exwm-workspace-number 1))
	  (exwm-workspace-switch (+ exwm-workspace-current-index 1))))

(defun exwm-workspace-prev ()
  (interactive)
  (if (> exwm-workspace-current-index 0)
	  (exwm-workspace-switch (- exwm-workspace-current-index 1))))


(general-define-key
 "M-h" 'exwm-workspace-next
 "M-l" 'exwm-workspace-prev)



;; (add-hook 'org-mode-hook 'magic-icon-fix)
;;;; Doom
(use-package doom-themes
	:init
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		  doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'ewal-doom-one t)

	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)

	;; Enable custom neotree theme (all-the-icons must be installed!)
	(doom-themes-neotree-config)
	;; or for treemacs users
	;(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
	;(doom-themes-treemacs-config)

	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config))

; (use-package treemacs)


(defun quantumize ()
  (interactive)
	(setq-local company-frontends '(company-preview-frontend))
	(setq-local company-minimum-prefix-length 0))


(use-package doom-modeline
  :init
  (setq doom-modeline-height 40)
  (doom-modeline-mode))

(use-package solaire-mode
  :init
  (solaire-global-mode))

(use-package centaur-tabs
  :init
  (setq centaur-tabs-height 16)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'left)
  (setq x-underline-at-descent-line t)
  (defun contextual-tabs ()
	(if (and (centaur-tabs-mode-on-p) (eq (derived-mode-p 'prog-mode) nil))
		(centaur-tabs-local-mode)))
  (centaur-tabs-mode)
  :hook
  (after-change-major-mode . contextual-tabs))

;;;; Dashboard
(use-package dashboard
  :init
  (setq dashboard-center-content t)
  (setq dashboard-disable-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-footer-messages '("The One True Editor!"
					"Protocol 3: Protect the Pilot"
					"All systems nominal."
					"Democracy... is non negotiable."
					"It's my way or... hell, it's my way!"
					"Death is a preferable alternative to communism."
					"Make life rue the day it though it could give Richard Stallman lemons!"
					"Vi-Vi-Vi, the editor of the beast."
					"Happy hacking!"
					"While any text editor can save your files, only Emacs can save you soul."
					"There's an Emacs package for that."
					"M-x butterfly"
					""))
  (setq dashboard-items '((recents  . 3)
						(projects . 3)
						(agenda . 5)))
  (setq dashboard-startup-banner 'official)
  (setq dashboard-page-separator "\n\n")
  (dashboard-setup-startup-hook)
  :hook
  (dashboard-mode . hide-mode-line-mode)
  (dashboard-mode . header-line-spacious)
  (dashboard-mode . turn-off-solaire-mode))

 ;;;; Exit
(setq exit-messages '(
	"Please don't leave, there's more demons to toast!"
	"Let's beat it -- This is turning into a bloodbath!"
	"I wouldn't leave if I were you. Vim is much worse."
	"Don't leave yet -- There's a demon around that corner!"
	"Ya know, next time you come in here I'm gonna toast ya."
	"Go ahead and leave. See if I care."
	"Are you sure you want to quit this great editor?"
	"Emacs will remember that."
	"Emacs, Emacs never changes."
	"Okay, look. We've both said a lot of things you're going to regret..."
	"You are *not* prepared!"
	"Look, bud. You leave now and you forfeit your body count!"
	"Get outta here and go back to your boring editors."
	"You're lucky I don't smack you for thinking about leaving."
	"Don't go now, there's a dimensional shambler waiting at the prompt!"
	"Just leave. When you come back I'll be waiting with a bat."
	"Are you a bad enough dude to stay?"
	"It was worth the risk... I assure you."
	"I'm willing to take full responsibility for the horrible events of the last 24 hours."
	))

(defun random-choice (items)
  (let* ((size (length items))
	 (index (random size)))
	(nth index items)))

(defun save-buffers-kill-emacs-with-confirm ()
  (interactive)
  (if (null current-prefix-arg)
	  (if (y-or-n-p (format "%s Quit? " (random-choice exit-messages)))
	(save-buffers-kill-emacs))
	(save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs-with-confirm)


;;;; Misc
(use-package page-break-lines
  :init (global-page-break-lines-mode))

(defun determine-olivetti ()
  (interactive)
  (olivetti-set-width (- (window-total-width) 8)))

;;; General Interface
;;;; Completing-Read
  (use-package prescient
	:init (setq prescient-persist-mode t))

  (use-package ivy
	:diminish
	:init
	(use-package amx :defer t)
	(use-package counsel :diminish :config (counsel-mode 1))
	(use-package swiper :defer t)
	(ivy-mode 1)
	:bind
	(("C-s"     . swiper-isearch)
	 ("M-x"     . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f"  . counsel-describe-function)
	 ("<f1> v"  . counsel-describe-variable)
	 ("<f1> o"  . counsel-describe-symbol)
	 ("<f1> l"  . counsel-find-library)
	 ("<f2> i"  . counsel-info-lookup-symbol)
	 ("<f2> u"  . counsel-unicode-char)
	 ("C-c g"   . counsel-git)
	 ("C-c o"   . ivy-omni-org)
	 ("C-c j"   . counsel-git-grep)
	 (:map ivy-minibuffer-map
		   ("C-r" . ivy-previous-line-or-history)
		   ("M-RET" . ivy-immediate-done)))
	 ;; (:map counsel-find-file-map
	 ;;		   ("C-~" . counsel-goto-local-home)))
	:custom
	(ivy-use-virtual-buffers t)
	(ivy-height 10)
	(ivy-on-del-error-function nil)
	(ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
	(ivy-count-format "[%d/%d] ")
	(ivy-wrap t)
	:config
	(defun counsel-goto-local-home ()
		"Go to the $HOME of the local machine."
		(interactive)
		(ivy--cd "~/")))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode))

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode))

(use-package ivy-prescient
  :after ivy prescient
  :init (ivy-prescient-mode))

;;;; Introspection
(use-package helpful
  :init
  (advice-add 'describe-function :override #'helpful-function)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-command :override #'helpful-callable)
  (advice-add 'describe-key :override #'helpful-key)
  (advice-add 'describe-symbol :override #'helpful-symbol)
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-functxion)
  (global-set-key (kbd "C-h C") #'helpful-command)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :hook
  (helpful-mode . hide-mode-line-mode)
  (helpful-mode-hook . determine-olivetti))



;;;; Movement
(use-package zygospore
  :bind ("s-m" . 'zygospore-toggle-delete-other-windows))

(defun opposite-other-window ()
  "Cycle buffers in the opposite direction."
  (interactive)
  (other-window -1))

(general-define-key
 "s-k" 'other-window
 "s-j" 'opposite-other-window)

;;;; Selection
(use-package expand-region)

(use-package selected
  :init
  (selected-global-mode)
  :bind (:map selected-keymap
			  ("u" . 'upcase-region)
			  ("d" . 'downcase-region)
			  ("w" . 'count-words-region)
			  ("e" . 'er/expand-region)
			  ("q" . 'selected-off)))

(use-package google-this
  :bind (:map selected-keymap
			  ("g" . 'google-this-region)))

(use-package move-text
  :init
  (bind-keys*
   ("M-<down>" . move-text-line-down)
   ("M-<up>" . move-text-line-up)))

;;;; Other
(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package hydra
  :bind ("C-c f" . hydra-flycheck/body))

(defhydra hydra-flycheck (:color blue)
  "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("M" flycheck-manual)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-mode)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))
;;; Useful Functions
(defun replace-in-string (what with in)
  "Replace substring (as WHAT) with another substring (as WITH) within a given string (as IN)."
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

;;; General Improvements
;;;; Vanilla
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;;;; Addons
(use-package outshine
  :hook (emacs-lisp-mode . outshine-mode))

(use-package marginalia
  :config (marginalia-mode))

(use-package embark
  :bind
  (("C-S-a" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none)))))

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line) ;; Move to beginning of text, not line.
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package yasnippet
  :init
  (setq yas-triggers-in-field t)
  (yas-global-mode))

(use-package goto-line-preview
  :init (general-define-key "M-g M-g" 'goto-line-preview
							"C-x n g" 'goto-line-relative-preview))

(use-package beacon
  :init (general-define-key "C-?" 'beacon-blink))

(use-package which-key
  :init (which-key-mode))



;;; Org
;;;; Basic Setup
(use-package org
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "NOPE(n)")))
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-link-elisp-confirm-function nil)
  (setq org-ellipsis " ")
  (setq org-link-frame-setup '((file . find-file)))
  (setq org-catch-invisible-edits 'error)
  (setq org-cycle-separator-lines 0)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-list-allow-alphabetical t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-emphasized-text t)
  (setq org-fontify-done-headline t)
  (setq org-modules (append org-modules '(org-habit org-id)))
  :bind
  ("C-c c" . org-capture)
  (:map org-mode-map
		("C-c C-k" . org-kill-note-or-show-branches)))
;;;;; Archiving
(setq org-directory "~/Dropbox/org")
(setq org-archive-location (concat org-directory "/archived.org::"))

(setq org-archive-truelocation (replace-in-string "~" (getenv "HOME") (concat org-directory "/archived.org")))

(setq org-archive-file-header-format "")
(add-hook 'before-save-hook 'org-archive-done-tasks)

(defun org-archive-done-tasks ()
  (interactive)
  (when (and (eq major-mode 'org-mode) (not (string= buffer-file-name org-archive-truelocation)))
	(org-map-entries
	 (lambda ()
	   (org-archive-subtree)
	   (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
	 "/DONE" 'file)
	(org-map-entries
	 (lambda ()
	   (org-archive-subtree)
	   (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
	 "/NOPE" 'file)
	(find-file (concat org-directory "/archived.org"))
	(write-file org-archive-truelocation)
	(previous-buffer)))

;;;; Project Structure

(defun parallel-project ()
"This function makes sure that the current heading has
(1) the tag :project:
(2) has any TODO keyword and
(3) a leading progress indicator"
	(interactive)
	(org-toggle-tag "project" 'on)
	(org-back-to-heading t)
	(let* ((title (nth 4 (org-heading-components)))
		   (keyword (nth 2 (org-heading-components))))
	   (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
		   (message "TODO keyword and progress indicator found")
		   )
	   (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
		   (message "no TODO keyword and no progress indicator found")
		   (forward-whitespace 1)
		   (insert "[/] ")
		   )
	   (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
		   (message "TODO keyword but no progress indicator found")
		   (forward-whitespace 2)
		   (insert "[/] ")
		   )
	   )
	)

(defun sequential-project ()
  (interactive)
  (parallel-project)
  (org-set-property "ORDERED" "t"))


;;;; Calendar

(defun open-org-yearview () (interactive)
  (call-process-shell-command "kitty -e 'agenda' &" nil 0))

(general-define-key "<f5>" 'open-org-yearview)

;;;; Capture
(setq org-capture-templates '(("t" "Todo [inbox]" entry
							   (file+headline "~/Dropbox/org/inbox.org" "Tasks")
							   "* TODO %i%?")
							  ("s" "Schedule" entry
							   (file+headline "~/Dropbox/org/schedule.org" "Schedule")
							   "* %i%? \n %U")
							  ))

(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 3)
			   ("~/Dropbox/org/schoolwork.org" :maxlevel . 2)
						   ("~/Dropbox/org/schedule.org" :maxlevel . 2)))


;;;; Project Review
(setq org-agenda-custom-commands
	  '(("p" tags "project" nil)
		("a" "My agenda"
		 ((org-agenda-list)
		  (org-agenda-list-stuck-projects)
		  (tags "project")))))
(setq org-stuck-projects
	  '("+project/-MAYBE-DONE" ("TODO") nil "\\<IGNORE\\>"))

;;;; Aesthetics


;;;;; Icons

(defun org-icons ()
   "Beautify Org Checkbox Symbol"
   (setq prettify-symbols-alist '(("TODO" . "")
								  ("DONE" . "")
								  ("WAIT" . "")
								  ("NOPE" . "")
								  ("[#A]" . "")
								  ("[#B]" . "")
								  ("[#C]" . "")
								  ("[ ]" . "")
								  ("[X]" . "")
								  ("[-]" . "")
								  ("#+BEGIN_SRC" . "")
								  ("#+END_SRC" . "―")
								  (":PROPERTIES:" . "")
								  (":END:" . "―")
								  ("#+STARTUP:" . "")
								  ("#+TITLE: " . "")
								  ("#+RESULTS:" . "")
								  ("#+NAME:" . "")
								  ("#+ROAM_TAGS:" . "")
								  ("#+FILETAGS:" . "")
								  ("#+HTML_HEAD:" . "")
								  ("#+SUBTITLE:" . "")
								  ("#+AUTHOR:" . "")
								  (":Effort:" . "")
								  ("SCHEDULED:" . "")
								  ("DEADLINE:" . "")))
   (prettify-symbols-mode))

(use-package org-superstar
  :init (add-hook 'org-mode-hook 'org-superstar-mode))
;; (use-package org-appear
;;   :straight (:host github :repo "awth13/org-appear")
;;   :init (add-hook 'org-mode-hook 'org-fragtog-mode))
(use-package org-fragtog
  :init (add-hook 'org-mode-hook 'org-fragtog-mode))
(use-package org-autolist
  :init (add-hook 'org-mode-hook 'org-autolist-mode))
;; (use-package org-marginalia
;;   :straight (:host github :repo "nobiot/org-marginalia")
;;   :init (add-hook 'org-mode-hook 'org-marginalia-mode)
;;   (defun org-marginalia-save-and-open (point)
;;	(interactive "d")
;;	(org-marginalia-save)
;;	(org-marginalia-open point))
;;   :bind (:map org-marginalia-mode-map
;;		 ("C-c n o" . org-marginalia-save-and-open)
;;		 ("C-c m" . org-marginalia-mark)
;;		 ("C-c n ]" . org-marginalia-next)
;;		 ("C-c n [" . org-marginalia-prev)))


(defun header-line-spacious ()
  (interactive)
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :height 150))

(defun header-line-spacious-custom (height)
  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :height height))


(defun writing-hook ()
  ""
  (setq-local company-backends '(company-wordfreq company-ispell))
  (setq-local company-transformers nil)
  (setq-local company-frontends '(company-preview-frontend))
  (setq-local company-minimum-prefix-length 0))

(add-hook 'org-mode-hook 'org-icons)
(add-hook 'org-mode-hook 'header-line-spacious)

;;;;; Faces

	(setq org-priority-faces '((?A . (:foreground "#f5381b" :weight 'bold))
							  (?B . (:foreground "#f5cb22"))
							  (?C . (:foreground "#6cad50"))))

	(setq org-todo-keyword-faces
		  '(("TODO" . (:foreground "#999999" :bold nil)) ("WAIT" . "#cfd1d1")
			("DONE" . "#6cad50") ("NOPE" . "#cfd1d1")))

	(defface org-checkbox-done-text
	  '((t (:foreground "#71696A" :strike-through t)))
	  "Face for the text part of a checked org-mode checkbox.")

(with-eval-after-load 'org
  (set-face-attribute 'org-hide nil
						:foreground "brightblack"
						:background nil)

	  (set-face-attribute 'org-ellipsis nil
						  :foreground "#999999"
						  :underline nil
						  :weight 'light)
	  (set-face-attribute 'org-special-keyword nil
						  :foreground "#999999"
						  :weight 'light)
	  (set-face-attribute 'org-document-title nil
						  :height 2.0
						  :weight 'bold)
	  (set-face-attribute 'org-todo nil
						  :weight 'light))
;;;;; No :PROPERTIES:
(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
			 (not (memq state '(overview folded contents))))
	(save-excursion
	  (let* ((globalp (memq state '(contents all)))
			 (beg (if globalp
					(point-min)
					(point)))
			 (end (if globalp
					(point-max)
					(if (eq state 'children)
					  (save-excursion
						(outline-next-heading)
						(point))
					  (org-end-of-subtree t)))))
		(goto-char beg)
		(while (re-search-forward org-drawer-regexp end t)
		  (save-excursion
			(beginning-of-line 1)
			(when (looking-at org-drawer-regexp)
			  (let* ((start (1- (match-beginning 0)))
					 (limit
					   (save-excursion
						 (outline-next-heading)
						   (point)))
					 (msg (format
							(concat
							  "org-cycle-hide-drawers:  "
							  "`:END:`"
							  " line missing at position %s")
							(1+ start))))
				(if (re-search-forward "^[ \t]*:END:" limit t)
				  (outline-flag-region start (point-at-eol) t)
				  (user-error msg))))))))))
(defun hide-wrapper ()
  (interactive)
  (org-cycle-hide-drawers 'all))
(global-set-key (kbd "s-b") 'hide-wrapper)

;;;;; Writing
(use-package flyspell
  :ensure nil
  :hook (org-mode . flyspell-mode))
(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

;;;;; Roam

(use-package org-roam-server
  :after (org-roam server)
  :config
  (setq org-roam-server-host "127.0.0.1"
		org-roam-server-port 8078
		org-roam-server-export-inline-images t
		org-roam-server-authenticate nil
		org-roam-server-network-label-truncate t
		org-roam-server-network-label-truncate-length 60
		org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
	"Ensure the server is active, then open the roam graph."
	(interactive)
	(org-roam-server-mode 1)
	(browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))


;;;; Babel
;; Enable Org Babel features
(org-babel-do-load-languages ;; More languages!
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (latex . t)
   (shell . t)
   (C . t)))
(setq org-confirm-babel-evaluate nil) ;; Don't ask me if I want to execute my code or not
(setq org-src-tab-acts-natively t) ;; Indentation fix

;; Enable org link features
(org-link-set-parameters
 "run"
 :follow #'org-babel-ref-resolve) ;; Allow execution of Org Babel code from links
(add-to-list 'org-file-apps '(directory . emacs)) ;; Allow links to open directories in Dired

;;; Programming
;;;; Sanity
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq indent-tabs-mode nil)

;;;; LSP
(use-package lsp-mode
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-completion-show-detail nil)
  :hook ((c++-mode . lsp)
		 (c-mode . lsp)
		 (python-mode . lsp)
		 (js-mode . lsp)
		 (typescript-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :after lsp
  :hook ((lsp-mode . lsp-ui-mode)))

;;;; Company
(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  (global-company-mode)
  :bind (:map company-active-map
			  ("RET" . 'company-complete-selection)))

(use-package company-quickhelp
  :after company
  :init (company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company-quickhelp)

(use-package company-prescient
  :after company prescient
  :init
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  :init (company-prescient-mode))


;;;; Language-specific
;;;;; C/C++ config
(setq c-default-style "k&r")

(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(use-package dtrt-indent
  :config
  (dtrt-indent-mode 1))

(setq company-tooltip-maximum-width 60)

(defun code-hook ()
  (lsp)
  (lsp-ui-mode)
  (lsp-ui-sideline-toggle-symbols-info)
  (lsp-ui-doc-mode)
  (company-mode)
  (company-quickhelp-mode)
  (olivetti-mode)
  (olivetti-set-width 180)
  (lsp-focus-mode)
  (focus-mode)
  ; (treemacs)
  (setq lsp-headerline-breadcrumb-mode 0))


(add-hook 'c-mode-common-hook 'code-hook)

(defun clean-whitespace-hook ()
  (whitespace-cleanup))
(add-hook 'before-save-hook #'clean-whitespace-hook)

;;;; Aesthetics
;;;;; hl-todo
(use-package hl-todo
  :init
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
		'(("TODO"   . "#99bb66")
		  ("FIXME"  . "#ff6655")
		  ("DEBUG"  . "#a9a1e1")
		  ("HACK"   . "#6c78dd")
		  ("NOTE"   . "#44b9b1")))
  ;; We already have todos in Org Mode!
  (add-hook 'org-mode-hook (lambda () (hl-todo-mode -1)))

  :bind (:map hl-todo-mode-map
  ("C-c p" . hl-todo-previous)
  ("C-c n" . hl-todo-next)
  ("C-c o" . hl-todo-occur)
  ("C-c i" . hl-todo-insert)))

;;;;; prettify-symbols

;;;;; Icons

(defun my/add-visual-replacement (from to)
  "Make `prettify-symbols-mode' replace string (as FROM) with string (as TO).

Updates `prettify-symbols-alist'.  You may need to toggle
`prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons from (let ((composition nil))
					 (dolist (char (string-to-list to)
								   (nreverse (cdr composition)))
					   (push char composition)
					   (push '(Br . Bl) composition))))
		prettify-symbols-alist))

(add-hook 'c-mode-common-hook
		  (lambda ()
			(my/add-visual-replacement "uint64_t" "u64")
			(my/add-visual-replacement "uint32_t" "u32")
			(my/add-visual-replacement "uint16_t" "u16")
			(my/add-visual-replacement "int8_t" "u8")
			(my/add-visual-replacement "int64_t" "i64")
			(my/add-visual-replacement "int32_t" "i32")
			(my/add-visual-replacement "int16_t" "i16")
			(my/add-visual-replacement "int8_t" "i8")
			(my/add-visual-replacement "size_t" "sz_t")
			(my/add-visual-replacement "->" "→")
			(my/add-visual-replacement ">=" "≥")
			(my/add-visual-replacement "<=" "≤")
			(my/add-visual-replacement "!=" "≠")))
(add-hook 'c++-mode-hook
		  (lambda ()
			(c-set-offset 'innamespace 0)
			(my/add-visual-replacement "Eigen::MatrixXf" "mXf")
			(my/add-visual-replacement "Eigen::MatrixXd" "mXd")
			(my/add-visual-replacement "Eigen::Vector2f" "v2f")
			(my/add-visual-replacement "Eigen::Vector2d" "v2d")
			(my/add-visual-replacement "Eigen::Vector2i" "v2i")
			(my/add-visual-replacement "Eigen::Vector3f" "v3f")
			(my/add-visual-replacement "Eigen::Vector3d" "v3d")
			(my/add-visual-replacement "Eigen::Vector3i" "v3i")
			(push '("std::" . "" ) prettify-symbols-alist)))


;;;; Compilation
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
  "C-x n s" 'narrow-to-defun-include-comments)

;;;; Dash

(defun minimal-browse-url (url)
  "Browse an arbitrary url (as URL) in a new frameless Firefox window."
  (split-window-right)
  (other-window 1)
  (call-process-shell-command (concat "firefox -P default-release --new-window " url) nil 0))

(use-package dash-docs)
(use-package counsel-dash
  :config
  (setq dash-docs-browser-func 'minimal-browse-url)
  (setq dash-docs-enable-debugging nil)
  (defun emacs-lisp-doc ()
	"Restrict dash docsets to Emacs Lisp."
	(interactive)
	(setq-local dash-docs-docsets '("Emacs Lisp")))
  (defun c-doc ()
	"Restrict dash docsets to C."
	(interactive)
	(setq-local dash-docs-docsets '("C")))
  (defun c++-doc ()
	"Restrict dash docsets to C/C++."
	(interactive)
	(setq-local dash-docs-docsets '("C" "C++")))
  (defun python-doc ()
	"Restrict dash docsets to Python."
	(interactive)
	(setq-local dash-docs-docsets '("Python 3")))
  :bind (:map prog-mode-map
			  ("C-c d" . 'counsel-dash)
			  ("C-c C-d" . 'counsel-dash-at-point))
  :hook
  (emacs-lisp-mode . emacs-lisp-doc)
  (c-mode . c-doc)
  (c++-mode . c++-doc)
  (python-mode-hook . python-doc))

;;; Writing
(setq ispell-program-name "aspell")
(define-key org-mode-map (kbd "C-c j") 'pandoc-jump-to-reference)
;;;; Distractionless
(defun make-clean-frame ()
  (interactive)
  (setq new-frame
		(make-frame
		 '((name . "editor")
		   (width . 80)
		 (height . 30)
		 (minibuffer . nil)
		 (top . 50)
		 (left . 0)
		 ))))
;;;; Hook
(defun word-processing-hook ()
  ;; Makes code buffers look nicer
  (olivetti-mode 1)
  (olivetti-set-width 100)
  (visual-line-mode 1)
  (global-set-key (kbd "C-c s-a") 'flyspell-auto-correct-word))

(add-hook 'org-mode-hook 'word-processing-hook)

;;; Gaps
;; TODO: Fix gaps
(add-hook 'exwm-init-hook (lambda () (load "exwm-outer-gaps")
							(exwm-outer-gaps-mode)
							(call-process-shell-command "bash ~/.config/polybar/launch.sh --docky" nil 0)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(esup compile zygospore writeroom-mode writegood-mode which-key web-completion-data wc-mode wc-goal-mode vterm use-package typescript-mode treepy svg-tag-mode sudo-edit solaire-mode smooth-scrolling smooth-scroll smartparens shackle selected rainbow-mode quickrun projectile powerthesaurus popper pfuture parrot outshine org-superstar org-roam-server org-gcal org-fragtog org-bullets org-autolist olivetti move-text mixed-pitch marginalia magit-todos lsp-ui lsp-origami lsp-ivy lsp-focus lively laas ivy-prescient ivy-posframe imenu-anywhere iedit hydra hide-mode-line helpful helm goto-line-preview google-this git-gutter-fringe general gcmh format-all flyspell-correct-ivy flycheck exwm-mff exwm-float expand-region ewal-doom-themes evil-collection embark dtrt-indent doom-modeline dired-rainbow diff-hl dashboard crux counsel-dash company-wordfreq company-quickhelp-terminal company-prescient company-flx company-box cmake-mode cfrs centered-window centaur-tabs ccls beacon avy apiwrap amx all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-dired ace-jump-mode))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
