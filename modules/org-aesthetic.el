;; TODO: Better icon solution
(defun org-icons ()
   "Beautify org mode keywords."
   (setq prettify-symbols-alist '(("TODO" . "")
								  ("WAIT" . "")
								  ("NOPE" . "")
								  ("DONE" . "")
								  ("[#A]" . "")
								  ("[#B]" . "")
 								  ("[#C]" . "")
								  ("[ ]" . "")
								  ("[X]" . "")
								  ("[-]" . "")
								  ("#+BEGIN_SRC" . "")
								  ("#+END_SRC" . "―")
								  (":PROPERTIES:" . "")
								  (":END:" . "―")
								  ("#+STARTUP:" . "")
								  ("#+TITLE: " . "")
								  ("#+RESULTS:" . "")
								  ("#+NAME:" . "")
								  ("#+ROAM_TAGS:" . "")
								  ("#+FILETAGS:" . "")
								  ("#+HTML_HEAD:" . "")
								  ("#+SUBTITLE:" . "")
								  ("#+AUTHOR:" . "")
								  (":Effort:" . "")
								  ("SCHEDULED:" . "")
								  ("DEADLINE:" . "")))
   (prettify-symbols-mode))

; (use-package mixed-pitch :hook (org-mode . mixed-pitch-mode))
(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "◈" "◎"))
  :init (add-hook 'org-mode-hook 'org-superstar-mode))
;; (use-package org-appear
;;   :straight (:host github :repo "awth13/org-appear")
;;   :init (add-hook 'org-mode-hook 'org-fragtog-mode))
(use-package org-fragtog
  :init (add-hook 'org-mode-hook 'org-fragtog-mode))
(use-package org-autolist
  :init (add-hook 'org-mode-hook 'org-autolist-mode))
;; (use-package org-marginalia
;;   :straight (:host github :repo "nobiot/org-marginalia")
;;   :init (add-hook 'org-mode-hook 'org-marginalia-mode)
;;   (defun org-marginalia-save-and-open (point)
;;	(interactive "d")
;;	(org-marginalia-save)
;;	(org-marginalia-open point))
;;   :bind (:map org-marginalia-mode-map
;;		 ("C-c n o" . org-marginalia-save-and-open)
;;		 ("C-c m" . org-marginalia-mark)
;;		 ("C-c n ]" . org-marginalia-next)
;;		 ("C-c n [" . org-marginalia-prev)))

	(setq org-priority-faces '((?A . (:foreground "#f5381b" :weight 'bold))
							  (?B . (:foreground "#f5cb22"))
							  (?C . (:foreground "#6cad50"))))

	;; (setq org-todo-keyword-faces
	;; 	  '(("TODO" . (:foreground "#999999" :bold nil)) ("WAIT" . "#cfd1d1")
	;; 		("DONE" . "#6cad50") ("NOPE" . "#cfd1d1")))

	(defface org-checkbox-done-text
	  '((t (:foreground "#71696A" :strike-through t)))
	  "Face for the text part of a checked org-mode checkbox.")

;; (general-def 'doc-view-mode-map
;;   "j" 'doc-view-next-page
;;   "k" 

(with-eval-after-load 'org
  (set-face-attribute 'org-hide nil
						:foreground "brightblack"
						:background nil)

	  (set-face-attribute 'org-ellipsis nil
						  :foreground "#999999"
						  :underline nil
						  :weight 'light)
	  (set-face-attribute 'org-special-keyword nil
						  :foreground "#999999"
						  :weight 'light)
	  (set-face-attribute 'org-document-title nil
						  :height 2.0
						  :weight 'bold)
	  (set-face-attribute 'org-todo nil
						  :weight 'light))

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
			 (not (memq state '(overview folded contents))))
	(save-excursion
	  (let* ((globalp (memq state '(contents all)))
			 (beg (if globalp
					(point-min)
					(point)))
			 (end (if globalp
					(point-max)
					(if (eq state 'children)
					  (save-excursion
						(outline-next-heading)
						(point))
					  (org-end-of-subtree t)))))
		(goto-char beg)
		(while (re-search-forward org-drawer-regexp end t)
		  (save-excursion
			(beginning-of-line 1)
			(when (looking-at org-drawer-regexp)
			  (let* ((start (1- (match-beginning 0)))
					 (limit
					   (save-excursion
						 (outline-next-heading)
						   (point)))
					 (msg (format
							(concat
							  "org-cycle-hide-drawers:  "
							  "`:END:`"
							  " line missing at position %s")
							(1+ start))))
				(if (re-search-forward "^[ \t]*:END:" limit t)
				  (outline-flag-region start (point-at-eol) t)
				  (user-error msg))))))))))
(defun hide-wrapper ()
  (interactive)
  (org-cycle-hide-drawers 'all))
(global-set-key (kbd "s-b") 'hide-wrapper)

