;; org-gtd.el --- GTD-like workflow for Org Mode

;; Define agenda files
(setq org-agenda-files '("~/Dropbox/org/inbox.org"
                         "~/Dropbox/org/projects.org"
                         "~/Dropbox/org/schedule.org"
                         "~/Dropbox/org/classes.org"
                         "~/Dropbox/org/extra.org"
                         "~/Dropbox/org/routine.org"
                         "~/Dropbox/org/schoolwork.org"
                         ))

;; Define custom agenda views (just a test for now)

(setq org-agenda-compact-blocks t)
(setq org-agenda-block-separator "")

;; Older custom view that didn't need org-super-agenda
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  (;(org-agenda-skip-function
                   ; '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "TODO"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'notdeadline))
                 (org-agenda-files '("~/Dropbox/org/schoolwork.org"))
                 (org-agenda-sorting-strategy '(deadline-up))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nDeadlines\n")))
          (todo "TODO"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-files '("~/Dropbox/org/projects.org" "~/Dropbox/org/schoolwork.org"))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          ;; (agenda nil
          ;;         ((org-agenda-entry-types '(:deadline))
          ;;          (org-agenda-format-date "")
          ;;          (org-deadline-warning-days 7)
          ;;          (org-agenda-skip-function
          ;;           '(org-agenda-skip-entry-if 'notregexp "\\* TODO"))
          ;;          (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))))))

;; GTD-ish capture templates
(setq org-capture-templates '(("t" "Todo" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("s" "Schedule" entry
                               (file+headline "~/Dropbox/org/schedule.org" "Schedule")
                               "* %i%? \n SCHEDULED: %U")
                              ("c" "Todo w/ context" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%? \n Context: %A")))
(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 2)
                           ("~/Dropbox/org/extra.org" :maxlevel . 2)
                           ("~/Dropbox/org/schedule.org" :maxlevel . 1)
                           ("~/Dropbox/org/schoolwork.org" :maxlevel . 1)))

(let ((org-super-agenda-groups
       '(
         (:name "Projects"
                :auto-parent(:file-path "projects"))
         (:name "Unprocessed"
                :file-path "inbox")
         )))
  (org-agenda nil "g"))

(global-set-key (kbd "C-c c") 'org-capture)
(defun agenda-wrapper ()
  (interactive)
  (org-agenda nil "a")
  (org-agenda-month-view))
(global-set-key (kbd "C-c a") 'agenda-wrapper)
(defun gtd-wrapper ()
  (interactive)
  (org-agenda nil "g")
  (org-agenda-day-view))
(global-set-key (kbd "C-c g") 'gtd-wrapper)
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " ")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day nil)
(provide 'org-gtd)


