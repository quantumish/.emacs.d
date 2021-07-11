(setq pocket-reader-archive-on-open nil)

(setq newsticker-url-list
	  '(("r/cpp-top" "https://reddit.com/r/cpp/top.rss?t=day")))
(defun sh-cmd ()
  (interactive)
  (call-process-shell-command "kitty unimatrix" nil 0))




(defun hackerman ()
  (interactive)
  (exwmsw-switch-to-right-screen)
  (zygospore-toggle-delete-other-windows)
  (call-process-shell-command "kitty unimatrix -s 95 -f" nil 0)
  (sleep-for 0.5) ;; HACK Literally what the fuck
  (exwmsw-switch-to-left-screen)
  (zygospore-toggle-delete-other-windows)
  (call-process-shell-command "kitty unimatrix -s 95 -f" nil 0)
  (sleep-for 0.2) ;; HACK AAAAAA
  (exwmsw-switch-to-center-screen)
  (magit-commit-create))

(general-def
  "C-c C-g" 'hackerman)

(general-def with-editor-mode-map
  "C-c C-c" (lambda () (interactive) (with-editor-finish 0)))

;; (defun unhackerman ()
;;   (interactive)
;;   (exwmsw-switch-to-right-screen)
;;   (zygospore-toggle-delete-other-windows)
;;   (kill-buffer)
;;   (exwmsw-switch-to-left-screen)
;;   (zygospore-toggle-delete-other-windows)
;;   (kill-buffer)
;;   (exwmsw-switch-to-center-screen))
