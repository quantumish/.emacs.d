;; org-gtd.el --- GTD-like workflow for Org Mode

;; Define agenda files
(setq org-agenda-files '("~/Dropbox/org/inbox.org"
                         "~/Dropbox/org/projects.org"
                         "~/Dropbox/org/schedule.org"))

;; Define "perspectives" that are seen throughout all agenda views
(setq org-super-agenda-groups '((:name "Today"
				:time-grid t
				:scheduled today)
			   (:name "Due today"
				:deadline today)
			   (:name "Important"
				:priority "A")
			   (:name "Overdue"
				:deadline past)
			   (:name "Due soon"
				:deadline future)
			   (:name "Waiting"
			       :todo "WAIT")))

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
(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 3)
                           ("~/Dropbox/org/schedule.org" :maxlevel . 2)))

(provide 'org-gtd)
