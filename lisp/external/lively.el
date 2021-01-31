‎‎(defun lively-update-overlay (o)
  "Evaluate the lively code for O and update its display text."
  (with-current-buffer (overlay-buffer o)
    (let ((expr (buffer-substring (overlay-start o) (overlay-end o))))
      (overlay-put o 'display (format "%s" (eval (read expr)))))))

(defun lively-init-timer ()
  "Setup background timer to update lively text."
  (setq lively-timer (run-with-timer 0 lively-interval 'lively-update)))

(defun lively-stop ()
  "Remove all lively regions in Emacs."
  (interactive)
  (when lively-timer (cancel-timer lively-timer))
  (setq lively-timer nil)
  (mapc 'delete-overlay lively-overlays)
  (setq lively-overlays nil))

;;; Nice to have:

(defun lively-shell-command (command)
  "Execute COMMAND and return the output, sans trailing newline."
  (let ((result (shell-command-to-string command)))
    (substring result 0 (1- (length result)))))
