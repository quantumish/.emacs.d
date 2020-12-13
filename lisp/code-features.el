;; ;; packages
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (package-initialize)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;   )

;; use-package lsp-pyright
 ;;  :ensure t
 ;;  :hook (python-mode . (lambda ()
 ;;                          (require 'lsp-pyright)
 ;;                          (lsp))))  ; or lsp-deferred

(with-eval-after-load 'company
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (company-tng-configure-default)
  (let ((map company-active-map))
    (define-key map (kbd "<tab>") 'company-complete-selection)
    (define-key map (kbd "RET") 'nil))
  (setq company-tooltip-limit 1) ;; Minimum is 3 :(

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
  
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package lsp-mode
  :hook (c-common-mode . lsp))

(use-package lsp-ui)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-indentation nil)
(use-package ccls
  :after projectile
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(defun code-features-hook ()
  ;; Makes code buffers look nicer
  (smartparens-mode))
(add-hook 'c-mode-common-hook 'code-features-hook)

;; (use-package google-c-style
;;   :hook ((c-mode c++-mode) . google-set-c-style)
;;          (c-mode-common . google-make-newline-indent))


(load "external/disaster.el")
;; TODO: Fix custom disaster.el toggle opcodes functions
(defun disaster-show-opcodes
    (interactive)
  (defcustom disaster-objdump "objdump -d -source -line-numbers"
    "The command name and flags for running objdump."
    :group 'disaster
    :type 'string)
  )
(defun disaster-hide-opcodes
    (interactive)
  (defcustom disaster-objdump "objdump -d -source -line-numbers --no-show-raw-insn"
    "The command name and flags for running objdump."
    :group 'disaster
    :type 'string)
  )
(with-eval-after-load 'c
  (define-key c-mode-base-map (kbd "C-c d") 'disaster)
  )

(provide 'code-features)
;;; code-features.el ends here