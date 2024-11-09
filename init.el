;;;; Emacs config: rebooted

;; Hopefully built to last!

;;; Setup

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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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

(setq use-package-enable-imenu-support t)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))




(use-package org :ensure t)

;; Better key bind management package.
(use-package general :ensure (:wait t) :demand t)

;;; Defaults

(use-package emacs :ensure nil
  :config
  ;; Turn off random GUI cruft
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)
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

  ;; Prevent any attempts to resize the frame.
  (setq frame-inhibit-implied-resize t)

  ;; Stop Emacs from trying to use dialog boxes.
  (setq use-dialog-box nil)

  ;; Prefer y/n over yes/no.
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Stuff is often here too.
  (add-to-list 'exec-path "~/.local/bin")

  ;; Visual preference.
  (global-visual-line-mode)

  ;; PgUp and PgDown are scarily close to arrow keys on my laptop.
  (when (string= system-name "qpad")
    (unbind-key "C-<prior>")
    (unbind-key "C-<next>")))

;;; EXWM

(use-package exwm :ensure t
    :config
    (setq exwm-workspace-number 1)
    (setq exwm-input-global-keys  `(([?\s-r] . exwm-reset)))
    (setq exwm-input-simulation-keys
          '(([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\C-s] . ?\C-f)
			([?\M-w] . ?\C-c)
			([?\C-y] . ?\C-v)
            ;; ([?\C-t] . ?\C-n)
            ([?\C-x?\C-s] . ?\C-s)
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete])))

    (setq exwm-layout-show-all-buffers t)
    (setq exwm-workspace-show-all-buffers t)
    ;; (setq exwm-workspace-minibuffer-position 'bottom)

    (defun b3n-exwm-set-buffer-name ()
      (if (and exwm-title (string-match "\\`http[^ ]+" exwm-title))
          (let ((url (match-string 0 exwm-title)))
            (setq-local buffer-file-name url)
            (setq-local exwm-title (replace-regexp-in-string
                                    (concat (regexp-quote url) " - ")
                                    ""
                                    exwm-title))))
      (setq-local exwm-title
                  (concat
                   exwm-class-name
                   "<"
                   (if (<= (length exwm-title) 50)
                       exwm-title
                     (concat (substring exwm-title 0 50) "…"))
                   ">"))

      (exwm-workspace-rename-buffer exwm-title))

    (defun opposite-other-window ()
      "Cycle buffers in the opposite direction."
      (interactive)
      (other-window -1))
    
    (defun opposite-other-frame ()
      "Cycle frames in the opposite direction."
      (interactive)
      (other-frame -1))


    ;; (defvar left-screen "HDMI-1")
    ;; (defvar middle-screen "eDP-1")
    ;; (require 'exwm-randr)
    ;; (setq exwm-randr-workspace-output-plist `(0 ,middle-screen 1 ,left-screen))
    ;; (exwm-randr-enable)

    
    :general
    (:keymaps '(override-global-map exwm-mode-map)
          "M-k" 'other-window
          "M-j" 'opposite-other-window
          "C-M-j" 'opposite-other-frame
          "C-M-k" 'other-frame)

    :hook
    ((exwm-update-class . b3n-exwm-set-buffer-name)
     (exwm-update-title . b3n-exwm-set-buffer-name)
     (after-init . exwm-init))

    :init
    (exwm-enable))

(general-def
  "<print>" '(lambda (x) (call-process-shell-command "flameshot gui" nil 0)))

(use-package exwm-outer-gaps
  :ensure (:host github :repo "walseb/exwm-outer-gaps")
  :config
  (defun exwm-outer-gaps-redraw ()
    "exwm-outer gaps sometimes has artifacts in the gap area. Quickly toggling the mode on and off works forces a redraw of the gaps and gets rid of them."
    (interactive)
    (exwm-outer-gaps-mode))
  (setq exwm-outer-gaps-width [25 25 25 0])
  :hook (exwm-init . exwm-outer-gaps-mode))

(use-package exwm-mff :ensure t
  :init (exwm-mff-mode))

;;; Completion


;; (use-package prescient :ensure t
;;   :init (setq prescient-persist-mode t))

;; (use-package ivy :ensure t
;;   :init
;;   (ivy-mode 1))

;; (use-package counsel :ensure t
;;   :config (counsel-mode 1)
;;   (setq counsel-search-engine 'google)
;;   :bind
;;   (("C-s"     . swiper-isearch)
;;    ("M-x"     . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-x C-l" . counsel-load-theme)
;;    ("C-h C-f" . counsel-faces)
;;    ("M-s g"   . counsel-search)
;;    ("M-g o"   . counsel-outline)
;;    ("M-g h"   . counsel-org-goto-all)
;;    ("M-g i"   . counsel-imenu)
;;    ("M-g a"   . counsel-linux-app)))

;; (use-package swiper :ensure t
;;  :bind (("C-s"     . swiper-isearch)))

;; (use-package ivy-prescient :ensure t
;;   :init (ivy-prescient-mode))

;; (use-package counsel-projectile :ensure t
;;   :bind
;;   (("M-g p"   . counsel-projectile-switch-project)))

;; (use-package ivy-rich :ensure t
;;   :after ivy
;;   :init (ivy-rich-mode)
;;   :config
;;   (setq ivy-rich-parse-remote-buffer nil)
;;   (setq ivy-rich-parse-remote-path nil))

;; (use-package nerd-icons-ivy-rich :ensure t
;;   :init (nerd-icons-ivy-rich-mode)
;;   :config
;;   (setq nerd-icons-ivy-rich-icon t)

;;   ;; Whether display the colorful icons.
;;   ;; It respects `nerd-icons-color-icons'.
;;   (setq nerd-icons-ivy-rich-color-icon t)

;;   ;; The icon size
;;   (setq nerd-icons-ivy-rich-icon-size 1.0)

;;   ;; Whether support project root
;;   (setq nerd-icons-ivy-rich-project t)

;;   ;; Maximum truncation width of annotation fields.
;;   ;; This value is adjusted depending on the `window-width'.
;;   (setq nerd-icons-ivy-rich-field-width 40)

;;   ;; Definitions for ivy-rich transformers.
;;   ;; See `ivy-rich-display-transformers-list' for details."
;;   nerd-icons-ivy-rich-display-transformers-list

;;   ;; Slow Rendering
;;   ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
;;   ;; you can try setting the following variable
;;   (setq inhibit-compacting-font-caches t)
;;   )

;; (use-package ivy-posframe :ensure t
;;   :init
;;   (ivy-posframe-mode)
;;   (setq ivy-posframe-height 11)
;;   (setq ivy-posframe-width 140)
;;   (setq ivy-posframe-border-width 1)
;;   (set-face-attribute 'ivy-posframe-border nil :background (doom-color 'fg)))

(use-package vertico :ensure t
  :init
  (require 'vertico-directory)
  (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)r)

(use-package vertico-prescient :ensure t
  :init (vertico-prescient-mode)) 

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
(use-package orderless :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :general (:keymaps 'minibuffer-local-map
             "M-A" 'marginalia-cycle)
  :init (marginalia-mode))

(use-package embark :ensure t
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
(use-package consult :ensure t
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

(use-package embark-consult :ensure t 
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; Help

(use-package helpful :ensure t
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

(use-package which-key :ensure t
  :init (which-key-mode))

;;; handy vanilla stuff

(use-package crux :ensure t
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

(use-package rainbow-mode :ensure t)

(use-package sudo-edit :ensure t)

(use-package drag-stuff :ensure t
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package emacs :ensure nil
  :general
  ("<pinch>" 'ignore)
  ("<C-wheel-up>" 'ignore)
  ("<C-wheel-down>" 'ignore))

;; terminals whee

(use-package emacs :ensure nil
  :init (require 'ansi-color)
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package vterm :ensure t
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

(use-package emacs :ensure nil
  :config
  (set-face-attribute 'default nil :family "UW Ttyp0"))

(use-package doom-themes :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  :init 
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package ewal :ensure t)
(use-package ewal-doom-themes :ensure t
  :init (load-theme 'ewal-doom-one t))

;; (use-package simple-modeline :ensure t
;;   :config
;;   (defun simple-modeline-segment-padding () " ")
;;   (setq simple-modeline-segments
;;      '((simple-modeline-segment-modified
;;         simple-modeline-segment-buffer-name
;;         simple-modeline-segment-position)
;;        (simple-modeline-segment-process
;;         simple-modeline-segment-major-mode
;;         simple-modeline-segment-padding)))  
;;   :init
;;   (simple-modeline-mode))


;; use-package with Elpaca:
(use-package dashboard
  :ensure t
  :hook 
  ((elpaca-after-init . dashboard-insert-startupify-lists)
   (elpaca-after-init . dashboard-initialize))
  :config
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startup-banner "~/Downloads/Group 1.png")
  (setq dashboard-image-banner-max-height 250)

  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5)))
  
  (setq dashboard-footer-messages
    '("The one true editor!"
      "Vi Vi Vi, the editor of the beast"
      "Any editor can save your files.\nOnly Emacs can save your soul."
      "M-x butterfly"
      "Eight Megabytes And Constantly Swapping"
      "Tiger got to hunt, Bird got to fly;\nLisper got to sit and wonder, (Y (Y Y))?\n\nTiger got to sleep, Bird got to land;\nLisper got to tell himself he understand."))

  (dashboard-setup-startup-hook))

(use-package doom-modeline :ensure t
  :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode))

(use-package nerd-icons-completion :ensure t
  :config (nerd-icons-completion-mode))

(use-package nerd-icons-dired :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; (use-package treemacs :ensure t)

;; (use-package treemacs-nerd-icons :ensure t
;;   :config
;;   (treemacs-load-theme "nerd-icons"))

(use-package solaire-mode :ensure t
  :hook (prog-mode . solaire-mode))

(use-package diredfl :ensure t
  :config (setq dired-listing-switches "-alh")
  :init (diredfl-global-mode))


;;; org mode

(use-package org
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
  
  :hook (org-mode . org-indent-mode))

;; (use-package org-variable-pitch :ensure t
;;   :init (org-variable-pitch-setup))

;; Auto-toggle emphasis
(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

;; Natural bulleted lists
(use-package org-autolist :ensure t
  :hook (org-mode . org-autolist-mode))

(use-package vanish
  :ensure (:host github :repo "thezeroalpha/vanish.el"))

(use-package org-roam :ensure t
  :config
  (setq org-roam-directory (file-truename "~/sync/notes"))
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

(use-package org-roam-ui
  :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
 ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


(use-package haskell-mode :ensure t)
(use-package go-mode :ensure t)
(use-package sml-mode :ensure t)
(use-package rust-mode :ensure t)

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

(use-package combobulate :ensure nil
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
        ;; combobulate-mode'.
    :hook
      ((python-ts-mode . combobulate-mode)
       (json-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/.emacs.d/combobulate"))


;; porcelain esque things

(use-package deadgrep :ensure t)
(use-package transient :ensure t)
(use-package magit :ensure t)
(use-package casual-dired :ensure t
  :general
  (:keymaps 'dired-mode-map
        "C-o" 'casual-dired-tmenu))  

;; misc cool shit

;; TODO mess with this
(use-package grugru :ensure t
  :general ("C-;" 'grugru)
  :config (grugru-default-setup))
(use-package vundo :ensure t)

;;; pdfs


(use-package hide-mode-line :ensure t
  :hook (exwm-mode . hide-mode-line-mode))

(use-package pdf-tools :demand t
  :ensure t
  :init
  (pdf-tools-install)
  (defun my/pdf-fit ()
    "Calculates the difference between the pdf image and frame width
     before resizing the frame by the difference"
    (interactive)
    (let ((W (- (car (pdf-view-image-size)) (window-pixel-width))))
      (window-resize (get-buffer-window) W t nil t)
      (pdf-view-fit-height-to-window)))

  :hook
  ((pdf-view-mode . pdf-view-midnight-minor-mode)
   (pdf-view-mode . hide-mode-line-mode)
   (pdf-view-after-change-page . my/pdf-fit)
   ))



(use-package app-launcher
  :ensure (:host github :repo "SebastienWae/app-launcher"))


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


(use-package org
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
(use-package engrave-faces :ensure t
  :config
  (setq org-latex-listings 'engraved)
  (setq org-latex-engraved-theme 'doom-one))

(use-package htmlize :ensure t)

(use-package org-special-block-extras :ensure t
  :hook (org-mode . org-special-block-extras-mode)
  :config
  (require 'org-special-block-extras)
  (org-defblock fancyquote (author "Unknown")
   (pcase backend
     (`latex (format "\\epigraph{%s}{\\emph{%s}}\\vspace{10pt}" contents author))
     (_ "")) ;; TODO handle HTML cases
   ))


(use-package ready-player
  :ensure (:host github :repo "xenodium/ready-player")
  :config (ready-player-mode))

(use-package lsp-mode :ensure t
                    ; :ensure-system-package ccls
                    ; :ensure-system-package (pyls . "python -m pip install pyls")
                    ; :ensure-system-package rust-analyzerf
  :init
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

(use-package lsp-ui :ensure t
  :after lsp
  :init
  (setq lsp-ui-doc-delay 5)
  (add-hook 'flycheck-mode-hook 'lsp-ui-mode) ;; HACK
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; (general-def
  ;;   :keymaps 'lsp-mode-map
  ;;   "C-c l p" 'lsp-ui-peek-find-references)
  :config
  (eval `(set-face-attribute 'lsp-ui-doc-background nil :background ,(doom-darken 'bg .2))))

;;;; Company
(use-package company :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (lsp-mode . company-mode))

(use-package flycheck :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package consult-flycheck :ensure t)

(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "NOPE(n)")))


(use-package idris2-mode
  :ensure (:host github :repo "idris-community/idris2-mode"))


(use-package xenops :ensure t
  :hook (org-mode . xenops-mode)
  :config (defun xenops-handle-paste ()))


(use-package yasnippet :ensure t
  :init (yas-global-mode))

(use-package aas :ensure t
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
(use-package laas :ensure t
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
(use-package olivetti :ensure t
  :init (setq-default olivetti-width 120)
  :hook (org-mode . olivetti-mode))

(c-set-offset 'innamespace 0)

(use-package dmenu :ensure t
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

(general-def
  :keymaps '(override-global-map exwm-mode-map)
  "M-d" (lambda () (interactive) (open-app "discord"))
  "M-f" (lambda () (interactive) (call-process-shell-command "firefox-bin --new-window" nil 0))
  "M-l" (lambda () (interactive) (call-process-shell-command "slock" nil 0))
  "M-s M-s" (lambda () (interactive) (call-process-shell-command "flameshot gui" nil 0)))

(set-face-attribute 'default nil :height 100)
(set-display-table-slot standard-display-table 0 ?\ )

(setq window-divider-default-right-width 20)
(setq window-divider-default-bottom-width 20)
(setq window-divider-default-places t)
(window-divider-mode)

(setq dash-docs-browser-func
      (lambda (x)
    (call-process-shell-command 
     (format "firefox-bin --new-window localhost:8000%s"
         (substring x (length "file:///")))
     nil 0)))


(use-package hl-todo :ensure t
  :config
  (setq hl-todo-keyword-faces
      `(("TODO"   . ,(doom-color 'green))
        ("FIXME"  . ,(doom-color 'red))
        ("DEBUG"  . ,(doom-color 'magenta))
        ("HACK"   . ,(doom-color 'violet))
        ("NOTE"   . ,(doom-color 'cyan))))
  :hook (prog-mode . hl-todo-mode))


(setq-default tab-width 4)
(add-to-list 'exec-path (format "%s/go/bin" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/.cargo/bin" (getenv "HOME")))
(add-to-list 'exec-path "/usr/local/go/bin")

(use-package consult-todo :ensure t) 

