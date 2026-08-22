;; -*- lexical-binding: t; -*-
;;; Configuration for system related functionality.

(use-package exwm
  :init
  (setq exwm-workspace-number 2)
  (setq exwm-input-global-keys  `(([?\s-r] . exwm-reset)))
  (setq exwm-input-simulation-keys
        '(([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-s] . ?\C-f)
          ([?\M-w] . ?\C-c)
          ([?\C-y] . ?\C-v)
          ([?\C-x?\C-s] . ?\C-s)
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))

  (setq exwm-layout-show-all-buffers t)
  (setq exwm-workspace-show-all-buffers t)

  (defun b3n-exwm-set-buffer-name ()
    (if (and (stringp exwm-title) (= (length (s-split " — " exwm-title)) 3))
        (cl-destructuring-bind (title url _) (s-split " — " exwm-title)
          (setq-local buffer-file-name url)
          (setq-local exwm-title (concat exwm-class-name "<" title ">")))
      (setq-local exwm-title
                  (concat
                   exwm-class-name
                   "<"
                   (if (<= (length exwm-title) 50)
                       exwm-title
                     (concat (substring exwm-title 0 50) "…"))
                   ">")))

    (exwm-workspace-rename-buffer exwm-title))

  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP" 1 "DisplayPort-1"))
  (exwm-randr-mode)

  (defun find-and-switch-to-app (app-name)
    "Looks for a buffer that probably corresponds to the window of an app."
    (let* ((predicate
            (lambda (b)
              (and (string-match-p (regexp-quote (format "%s |" app-name)) (buffer-name b))
                   (eq (buffer-local-value 'major-mode b) 'exwm-mode))))
           (cands (seq-filter predicate (buffer-list))))
      (if (car cands)
          (or (switch-to-buffer (car cands)) t))))

  (defun open-discord ()
    "Switches to any open EXWM buffer of Discord or opens it."
    (interactive)
    (if (not (find-and-switch-to-app "discord"))
        (call-process-shell-command
         "env MOZ_USE_XINPUT2=1 firefox --new-window discord.com/app" nil 0)))

  (defun bind-exwm-key (keystr func)
    (define-key override-global-map (kbd keystr) func)
    (define-key exwm-mode-map (kbd keystr) func))

  (defmacro bind-named-cmd (keystr name cmd)
    "Helper macro to define a wrapper around a shell command and bind
     it to a key in both the global map and the EXWM map.

     This is useful because it means that describe-key can meaningfully
     describe what is bound to the key (instead of just pointing to an
     anonymous function)."
    `(progn
       (defun ,name ()
         (interactive)
         (call-process-shell-command ,cmd nil 0))
       (bind-exwm-key ,keystr ',name)))

  ;; TODO consider warning in the event these aren't found?
  (when (executable-find "brightnessctl")
    (bind-named-cmd "<XF86MonBrightnessUp>" brightness-up-10 "brightnessctl set 10%+")
    (bind-named-cmd "<XF86MonBrightnessDown>" brightness-down-10 "brightnessctl set 10%-"))

  (when (executable-find "amixer")
    (bind-named-cmd "<XF86AudioRaiseVolume>" volume-up-5 "amixer set Master 5%+")
    (bind-named-cmd "<XF86AudioLowerVolume>" volume-down-5 "amixer set Master 5%-")
    (bind-named-cmd "<XF86AudioMute>" volume-toggle "amixer set Master toggle"))

  (when (executable-find "firefox")
    (bind-named-cmd "M-f" firefox-new-tab "env MOZ_USE_XINPUT2=1 firefox --new-window")
    (bind-exwm-key "M-d" 'open-discord))

  (when (executable-find "slock")
    (bind-named-cmd "M-l" lock-screen "slock"))

  (when (executable-find "flameshot")
    (bind-named-cmd "M-s M-s" flameshot
                    "flameshot gui -r | xclip -selection clipboard -t image/png"))

  (bind-exwm-key "M-k" 'other-window)
  (bind-exwm-key "M-j" (lambda () (interactive) (other-window -1)))
  (bind-exwm-key "C-M-j" 'other-frame)
  (bind-exwm-key "C-M-k" (lambda () (interactive) (other-frame -1)))

  (exwm-enable)
  
  :hook
  ((exwm-update-class . b3n-exwm-set-buffer-name)
   (exwm-update-title . b3n-exwm-set-buffer-name)))

(use-package dmenu
  :init (bind-exwm-key "M-p" 'dmenu))

(use-package prodigy :demand t
  :config
  (setq prodigy-kill-process-buffer-on-stop t)

  (defun my/prodigy-define-services ()
    (prodigy-define-service
      :name "earlyOOM" :command "earlyoom" :args '("-r" "0")
      :tags '(stability) :auto-start t)
    (prodigy-define-service
      :name "Unclutter" :command "unclutter"
      :tags '(x11) :auto-start t)
    (prodigy-define-service
      :name "Picom" :command "picom"
      :tags '(x11)  :auto-start t)

    (dolist (x (seq-filter #'stringp exwm-randr-workspace-monitor-plist))
      (prodigy-define-service
        :name (format "Polybar (%s)" x) :command "polybar"
        :args (list "-q" "main" "-c" (concat (getenv "HOME") "/.config/polybar/shades/config.ini"))
        :env (list (list "MONITOR" x))
        :tags '(theme) :auto-start t)))

  (defun prodigy-autostart ()
    "Start all unstarted services with `:auto-start' set to non-nil."
    (interactive)
    (prodigy-with-refresh
     (--each prodigy-services
       (when (and (plist-get it :auto-start)
                  (not (prodigy-service-started-p it)))
         (prodigy-start-service it)))))

  (defun prodigy-killall ()
    (interactive)
    (prodigy-with-refresh
     (--each prodigy-services (prodigy-stop-service it))))

  :hook ((exwm-init . (lambda () (my/prodigy-define-services) (prodigy-autostart)))
         (kill-emacs . prodigy-killall)))

(use-package exwm-outer-gaps
  :ensure (:host github :repo "walseb/exwm-outer-gaps")
  :config
  (defun exwm-outer-gaps-redraw ()
    "exwm-outer gaps sometimes has artifacts in the gap area.
     Quickly toggling the mode on and off works forces a redraw of
     the gaps and gets rid of them."
    (interactive)
    (exwm-outer-gaps-mode))
  (setq exwm-outer-gaps-width [25 25 25 0])
  :hook (exwm-init . exwm-outer-gaps-mode))

(use-package exwm-mff
  :hook (exwm-init . exwm-mff-mode))
