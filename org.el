;; I want to be able to complete tasks

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

;; Sequential projects

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t)

;; Recurring todos

(setq org-modules (append org-modules '(org-habit)))

;; Forecast / Agenda


;; Calendar

;; Notifications

;; Perspectives

;; Org-capture

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("s" "Schedule" entry
                               (file+headline "~/Dropbox/org/schedule.org" "Schedule")
                               "* %i%? \n %U")
                              ))

(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 3)
                           ("~/Dropbox/org/schedule.org" :maxlevel . 2)))

;; Tags

;; Project review

;; Focus mode (Only one type of work)

;; Live editing?
