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
  (org-set-property "ORDERED" "t"))

(setq org-agenda-prefix-format
	  '((agenda . " %?-12t% s")
	   (todo . " %-12:c")
	   (tags . " ")
	   (search . " %-12:c")))

(setq org-agenda-custom-commands
	  '(("p" tags "project" nil)
		("a" "My agenda"
		 ((org-agenda-list)
		  (org-agenda-list-stuck-projects)
		  (tags "project")))))

(setq org-stuck-projects
	  '("+project/-MAYBE-DONE" ("TODO") nil "\\<IGNORE\\>"))
