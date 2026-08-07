;; -*- lexical-binding: t; -*-
;;; Configuration for IDE-related functionality.

(setq-default tab-width 4)
(setq indent-tabs-mode nil)
(add-to-list 'exec-path (format "%s/go/bin" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/.cargo/bin" (getenv "HOME")))
(add-to-list 'exec-path "/usr/local/go/bin")

(c-set-offset 'innamespace 0)

(use-package deadgrep)
(use-package transient)
(use-package magit)

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
      `(("TODO"   . ,(doom-color 'green))
        ("FIXME"  . ,(doom-color 'red))
        ("DEBUG"  . ,(doom-color 'magenta))
        ("HACK"   . ,(doom-color 'violet))
        ("NOTE"   . ,(doom-color 'cyan))))
  :hook (prog-mode . hl-todo-mode))

(use-package consult-todo)

(use-package haskell-mode)
(use-package go-mode)
(use-package sml-mode)
(use-package rust-mode)

(use-package lsp-mode
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
  :hook ((c++-mode . lsp)
         (python-mode . lsp)
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

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (lsp-mode . company-mode))


(use-package eros
  :config
  (defun slime-eval-last-expression-eros ()
    (interactive)
    (cl-destructuring-bind (output value)
        (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
                                 :where (point)
                                 :duration eros-eval-result-duration))))

(use-package slime
  :after eros
  :config
  (setq inferior-lisp-program "sbcl")

  :bind (:map slime-mode-map
              ("C-x C-e" . slime-eval-last-expression-eros))
  :hook ((lisp-mode . slime)
         (lisp-mode . slime-mode)))

(use-package eat :ensure
  (:host codeberg
         :repo "akib/emacs-eat"
         :files ("*.el" ("term" "term/*.el") "*.texi"
                 "*.ti" ("terminfo/e" "terminfo/e/*")
                 ("terminfo/65" "terminfo/65/*")
                 ("integration" "integration/*")
                 (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (setq eat-shell-prompt-annotation-position 'right-margin)
  (setq eat-shell-prompt-annotation-failure-margin-indicator "⛌ ")
  (setq eat-shell-prompt-annotation-success-margin-indicator "✓ ")
  (setq eat-shell-prompt-annotation-running-margin-indicator "⋯ "))
