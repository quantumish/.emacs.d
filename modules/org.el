;; -*- lexical-binding: t; -*-
;;; Configuration for Org mode related functionality

(use-package org :ensure nil
  :config
  ;; visual settings
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
  ;; export settings
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
          ("parfill" "parskip" nil)))
  :hook (org-mode . org-indent-mode))

;; Auto-toggle emphasis
(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

;; Natural bulleted lists
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

(use-package xenops
  :hook (org-mode . xenops-mode)
  :config (defun xenops-handle-paste ()))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode)))

(use-package olivetti
  :config (setq-default olivetti-width 120)
  :hook (org-mode . olivetti-mode))
