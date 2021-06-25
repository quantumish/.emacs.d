(require 'exwm)
(require 'exwm-config)
(exwm-config-example)
(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(0 "HDMI-0" 1 "DP-1" 2 "DP-3" 3 ))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
		(start-process-shell-command
		 "xrandr" nil "xrandr --output DP-1 --output HDMI-0 --output DP-3 --auto")))
(exwm-randr-enable)

(defun exwm-workspace-next ()
  (interactive)
  (if (< exwm-workspace-current-index (- exwm-workspace-number 1))
	  (exwm-workspace-switch (+ exwm-workspace-current-index 1))))

(defun exwm-workspace-prev ()
  (interactive)
  (if (> exwm-workspace-current-index 0)
	  (exwm-workspace-switch (- exwm-workspace-current-index 1))))

(general-define-key
 "M-h" 'exwm-workspace-next
 "M-l" 'exwm-workspace-prev)

(use-package exwm-mff
  :init (exwm-mff-mode))

(load "exwmsw")
(setq exwmsw-active-workspace-plist '("HDMI-0" 0 "DP-3" 0 "DP-1" 0))
(setq exwmsw-the-left-screen "DP-3")
(setq exwmsw-the-center-screen "HDMI-0")
(setq exwmsw-the-left-screen "DP-1")
(general-def override-global-map
  "C-M-j" #'exwmsw-cycle-screens
  "C-M-k" #'exwmsw-cycle-screens-backward)

(general-def exwm-mode-map
  "C-M-j" #'exwmsw-cycle-screens
  "C-M-k" #'exwmsw-cycle-screens-backward)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

(defun b3n-exwm-set-buffer-name ()
  (if (and exwm-title (string-match "\\`http[^ ]+" exwm-title))
	(let ((url (match-string 0 exwm-title)))
	  (setq-local buffer-file-name url)
	  (setq-local exwm-title (replace-regexp-in-string
							  (concat (regexp-quote url) " - ")
							  ""
							  exwm-title))))
  (setq-local exwm-title
			  (concat
			   exwm-class-name
			   "<"
			   (if (<= (length exwm-title) 50)
				   exwm-title
				 (concat (substring exwm-title 0 50) "…"))
			   ">"))

  (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-class-hook 'b3n-exwm-set-buffer-name)
(add-hook 'exwm-update-title-hook 'b3n-exwm-set-buffer-name)
