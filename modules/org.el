;; TODO: Properly separate org config.

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
  (setq org-hide-leading-stars t)
  (setq org-modules (append org-modules '(org-habit org-id)))
  (setq org-agenda-block-separator " ")
  (setq org-agenda-start-day "0d")
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-files '("~/sync/org/inbox.org"
						   "~/sync/org/schoolwork.org"
						   "~/sync/org/extra.org"
						   "~/sync/org/projects.org"))
  :bind
  ("C-c c" . org-capture)
  (:map org-mode-map
		("C-c C-k" . org-kill-note-or-show-branches))
  :hook
  (org-mode . org-indent-mode))
