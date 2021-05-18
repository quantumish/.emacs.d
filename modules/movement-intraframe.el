(use-package zygospore
  :bind ("M-m" . 'zygospore-toggle-delete-other-windows))

(defun opposite-other-window ()
  "Cycle buffers in the opposite direction."
  (interactive)
  (other-window -1))

(general-def 'override-global-map
 "M-k" 'other-window
 "M-j" 'opposite-other-window)

(general-def 'exwm-mode-map
 "M-k" 'other-window
 "M-j" 'opposite-other-window)

