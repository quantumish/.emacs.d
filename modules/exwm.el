(use-package exwm)
(require 'exwm)
(setq exwm-workspace-number 3)
(setq exwm-input-global-keys
      `(
        ;; 's-r': Reset (to line-mode).
        ([?\s-r] . exwm-reset)
        ;; 's-w': Switch workspace.
        ([?\s-w] . exwm-workspace-switch)
        ;; 's-&': Launch application.
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; 's-N': Switch to certain workspace.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))
(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))
(exwm-enable)
(require 'exwm-randr)

(defvar left-screen "DP-1")
(defvar middle-screen "HDMI-0")
(defvar right-screen "DP-3")
(setq exwm-randr-workspace-output-plist `(0 ,middle-screen 1 ,left-screen 2 ,right-screen))
(add-hook 'exwm-randr-screen-change-hook
	  (lambda ()
		(start-process-shell-command
		 "xrandr" nil (concat "xrandr --output " left-screen " --output " middle-screen " --output " right-screen " --auto"))))
(exwm-randr-enable)

(setq exwm-layout-show-all-buffers t)
(setq exwm-workspace-show-all-buffers t)

(if (eq system-type 'gnu/linux)
	(progn
	  (use-package exwm
		:ensure-system-package (xbindkeys xcape dunst flameshot unclutter polybar feh))
	  (call-process-shell-command "xmodmap ~/.config/X/.xmodmap" nil 0)
	  (call-process-shell-command "xbindkeys" nil 0)
	  (call-process-shell-command "sh ~/.config/X/xcape.sh" nil 0)
	  (call-process-shell-command "dunst &" nil 0)
	  (call-process-shell-command "sh ~/.config/dunst/reload_dunst.sh" nil 0)
	  (call-process-shell-command "unclutter &" nil 0)
	  (call-process-shell-command "flameshot &" nil 0)
	  ;; TODO Randomly decides to reinstall things sometimes
	  ;; (use-package exwm
	  ;; 	:ensure-system-package (rustup cmake python38 python38-pip)
	  ;; 	:ensure-system-package (syncthing activitywatch)
	  ;; 	:ensure-system-package (firefox kitty discord spotify steam dropbox zathura pavucontrol) ; intel-vtune-profiler
	  ;; 	:ensure-system-package (lsd rm-improved fd bat hyperfine gotop unzip tig) ; ripgrep
	  ;; 	:ensure-system-package (slock xclip rofi mpd mpv texlive-most) ; pandoc-bin
	  ;; 	:ensure-system-package (neofetch unimatrix pipes.sh))
	  ))


(add-hook 'exwm-init-hook
		  (lambda ()
			(start-process-shell-command
			 "xrandr" nil (concat "xrandr --output " left-screen " --rotate left"))))

	  
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
(setq exwmsw-active-workspace-plist `(,middle-screen 0 ,right-screen 0 ,left-screen 0))
(setq exwmsw-the-right-screen right-screen)
(setq exwmsw-the-center-screen middle-screen)
(setq exwmsw-the-left-screen left-screen)
(general-def override-global-map
  "C-M-j" #'exwmsw-cycle-screens
  "C-M-k" #'exwmsw-cycle-screens-backward)

(general-def exwm-mode-map
  "C-M-j" #'exwmsw-cycle-screens
  "C-M-k" #'exwmsw-cycle-screens-backward)

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

(start-process-shell-command "polybar-update" nil
    (concat "sed s/<MONITOR>/"
			middle-screen
			"/g -i ~/.config/polybar/config.ini.bak > ~/.config/polybar/config.ini"))
