;;; paranoia.el --- paranoid hiding of Emacs buffers/files

;;; Commentary:
;;
;; AIVs be crazy.
;;

;;; Code:
;;; -*- lexical-binding: t; -*-
(require 'redacted)
(require 'cl-lib)

(defvar paranoia-files '()
  "List of all files with paranoia.")

(defvar paranoia-buffers '()
  "List of all open buffers with paranoia.")

(defvar paranoia-peek-time 5
  "Time to wait before re-obscuring buffer after peek.
See documentation for `run-at-time` for what values are valid.")

(defun paranoia-peek ()
  "Quickly peek at a paranoid buffer."
  (interactive)
  (redacted-mode 0)
  (run-at-time paranoia-peek-time nil (lambda () redacted-mode 1)))
               ;; (let ((buf (current-buffer)))
               ;;   (lambda () (with-current-buffer buf (redacted-mode 1))))))

(defun paranoia--setup-current-buffer ()
  "Enable paranoia-mode for `current-buffer`."
  (redacted-mode 1)
  (setq lexical-binding t)
  (if (not (member (current-buffer) paranoia-buffers))
      (add-to-list 'paranoia-buffers (current-buffer)))
  (if (and (buffer-file-name (current-buffer))
           (not (member (buffer-file-name (current-buffer)) 'paranoia-files)))
      (add-to-list 'paranoia-files (buffer-file-name (current-buffer)))))

(defun paranoia--teardown-current-buffer ()
  "Disable paranoia-mode for `current-buffer`.
Assumes that `paranoia--setup-current-buffer` has been run."
  (redacted-mode 0)
  (setq paranoia-buffers (delete (current-buffer) paranoia-buffers)))

(defun paranoia-ensure-none-visible ()
  "Redacts all visible buffers in `paranoia-buffers`."
  (dolist (buf (cl-remove-if-not #'get-buffer-window paranoia-buffers))
    (with-current-buffer buf
      (redacted-mode 1))))
  
(define-minor-mode paranoia-mode
  "Toggle paranoia for buffer."
  :init-value nil
  :lighter " paranoia"
  :keymap
  `((,(kbd "C-c x") . paranoia-peek))
  (if paranoia-mode
      (paranoia--setup-current-buffer)
    (paranoia--teardown-current-buffer)))

(provide 'paranoia)
;;; paranoia.el ends here
