(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file)
  (save-buffer)
  (set-buffer "archived.org")
  (save-buffer))

(setq org-directory "~/Dropbox/org")
(setq org-archive-location (concat org-directory "/archived.org::"))
(setq org-archive-file-header-format "")

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t)
