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

(require 'ccls)
(setq ccls-executable "/usr/local/bin/ccls")
;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)

;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

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
