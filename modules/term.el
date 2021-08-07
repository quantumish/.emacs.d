(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun dw/eshell-prompt ()  
  (concat
   "\n"
   (propertize (system-name) 'face `(:foreground "#3975D3") 'read-only t)
   (propertize " " 'face `(:foreground "white") 'read-only t)
   (propertize (dw/get-prompt-path) 'face `(:foreground "#4F8DBB") 'read-only t)
   (propertize " • " 'face `(:foreground "white") 'read-only t)
   (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#71899b") 'read-only t)
   (if (= (user-uid) 0)
       (propertize "\n#" 'face `(:foreground "red2") 'read-only t)
     (propertize "\nλ" 'face `(:foreground "#71899b") 'read-only t))
   (propertize " " 'face `(:foreground "#a2c5de"))
   ))

(defun dw/eshell-configure ()
  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
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

(use-package eshell
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

;; (use-package eshell-info-banner
;;   :straight (eshell-info-banner :type git :host github
;; 								:repo "phundrak/eshell-info-banner.el")
;;   :hook (eshell-banner-load . eshell-info-banner-update-banner))

(use-package eshell-manual
  :straight (eshell-manual :type git :host github
 						   :repo "nicferrier/eshell-manual"))

;; (use-package eshell-fringe-status
;;   :init
;;   (setq eshell-fringe-status-success-bitmap 'my-flycheck-fringe-indicator)
;;   (setq eshell-fringe-status-failure-bitmap 'my-flycheck-fringe-indicator)
;;   :hook (eshell-mode . eshell-fringe-status-mode))

;; (use-package esh-help
;;   :init (setup-esh-help-eldoc))
