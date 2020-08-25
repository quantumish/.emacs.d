;; org-gtd.el --- GTD-like workflow for Org Mode

;; Define agenda files
(setq org-agenda-files '("~/Dropbox/org/inbox.org"
                         "~/Dropbox/org/projects.org"
                         "~/Dropbox/org/schedule.org"
                         "~/Dropbox/org/classes.org"
                         "~/Dropbox/org/routine.org"
                         "~/Dropbox/org/schoolwork.org"
                         ))

;; Define "perspectives" that are seen throughout all agenda views
(setq org-super-agenda-groups '(
                                (:name "Burn"
                                       :effort< 60)
                                ))

;; Define custom agenda views
(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")))))

;; GTD-ish capture templates
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("s" "Schedule" entry
                               (file+headline "~/Dropbox/org/schedule.org" "Schedule")
                               "* %i%? \n %U")))
(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 2)
                           ("~/Dropbox/org/schedule.org" :maxlevel . 1)
                           ("~/Dropbox/org/schoolwork.org" :maxlevel . 1)))

;(global-set-key (kbd "C-c c") 'org-capture)

(provide 'org-gtd)
