;; -*- lexical-binding: t; -*-
;;; Configuration for the Vertico (VOMPEC) completion stack.

(use-package vertico
  :init (vertico-mode)
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Prior to Emacs 31, a minibuffer prompt from completing-read-multiple did
;; nothing to indicate that multiple options could be selected. This is a
;; patch from the author of vertico.el to work around it.
;; Displays [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(when (< emacs-major-version 31)
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (string-replace "[ \t]*" "" crm-separator)
                              (car args))
                      (cdr args)))))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

;; Hide commands in M-x which do not work in the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package prescient)
(use-package vertico-prescient
  :init (vertico-prescient-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package consult
  :bind ;; TODO bind the other commands
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x p b" . consult-project-buffer)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-fd)
   ("M-s r" . consult-ripgrep)
   ("C-s" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))
