;; -*- lexical-binding: t; -*-
;;; Configuration relating to default behavior of Emacs.

;; Make Emacs never beep.
(setq ring-bell-function #'ignore)
(setf bell-fn #'ignore)

;; Not personally a fan of customize.el
(setq custom-file "/dev/null")
(setq package-selected-packages "/dev/null/")

;; These keybinds suspend Emacs (in order to mimic terminal behavior).
;; This has *only* caused me trouble in GUI Emacs.
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;; Stop making backup files everywhere, put them all in one place!
(make-directory "~/.saves/" t)
(make-directory "~/.autosaves/" t)
(setq auto-save-file-name-transforms '((".*" "~/.autosaves/" t)))
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Stop Emacs from bothering you about disabled commands.
(setq disabled-command-function nil)

;; Stop Emacs from trying to use dialog boxes.
(setq use-dialog-box nil)

;; Prefer y/n over yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Stuff is often here too.
(add-to-list 'exec-path "~/.local/bin")

;; Visual preference.
(global-visual-line-mode)

;; PgUp and PgDown are scarily close to arrow keys on my laptop.
(when (string= (system-name) "qpad")
  (unbind-key "C-<prior>")
  (unbind-key "C-<next>"))

;; I don't like the character that is displayed when a line goes over
(set-display-table-slot standard-display-table 0 ?\ )

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line)
   ("C-<tab>" . crux-indent-defun))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package rainbow-mode)

(setq large-file-warning-threshold 100000000)

(use-package sudo-edit)

(use-package drag-stuff
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<touchscreen-pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
