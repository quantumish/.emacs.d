(add-hook 'emacs-startup-hook
  (lambda ()
	(setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  "Disable garbage collection at init."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Restore garbage collection after init."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)
(use-package gcmh
  :init
  (setq gcmh-idle-delay 5
		gcmh-high-cons-threshold (* 16 1024 1024) ))

