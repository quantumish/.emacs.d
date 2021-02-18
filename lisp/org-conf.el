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

(defun parallel-project ()
"This function makes sure that the current heading has
(1) the tag :project:
(2) has any TODO keyword and
(3) a leading progress indicator"
    (interactive)
    (org-toggle-tag "project" 'on)
    (org-back-to-heading t)
    (let* ((title (nth 4 (org-heading-components)))
           (keyword (nth 2 (org-heading-components))))
       (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
           (message "TODO keyword and progress indicator found")
           )
       (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
           (message "no TODO keyword and no progress indicator found")
           (forward-whitespace 1)
           (insert "[/] ")
           )
       (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
           (message "TODO keyword but no progress indicator found")
           (forward-whitespace 2)
           (insert "[/] ")
           )
       )
    )

(defun sequential-project ()
  (interactive)
  (parallel-project)
  (org-set-property "ORDERED" "t")
  )

;; Recurring todos

(setq org-modules (append org-modules '(org-habit)))

;; Forecast / Agenda

;; Calendar
;; Done! See rougier's great agenda script

;; Dashboard

;; Notifications
;; Done? External python script sort of works.
;; Just in case, run it on startup.
;; (shell-command "python ~/.emacs.d/notif.py &")

;; Perspectives
;; Maybe not?

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


(global-set-key (kbd "C-c c") 'org-capture)

;; Project review

(setq org-agenda-custom-commands
      '(("p" tags "project" nil)
        ("a" "My agenda"
         ((org-agenda-list)
          (org-agenda-list-stuck-projects)
          (tags "project")))))
(setq org-stuck-projects
      '("+project/-MAYBE-DONE" ("TODO") nil "\\<IGNORE\\>"))
