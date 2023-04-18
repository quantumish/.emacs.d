(add-to-list 'display-buffer-alist '("*intro-music*" display-buffer-no-window (nil)))

(add-hook 'exwm-randr-refresh-hook  (lambda ()
  (other-frame 1)
  (split-window-below -30)
  (other-window 1)
  (switch-to-buffer "kitty<>")
  (other-frame -1)

  (async-shell-command
   "aplay /home/quantumish/Downloads/beachside.wav" 
   (get-buffer-create "*intro-music*"))))
