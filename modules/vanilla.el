(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))

(defun magic-icon-fix ()
  (let ((fontset (face-attribute 'default :fontset)))
	(set-fontset-font fontset '(?\xf000 . ?\xf2ff) "FontAwesome" nil 'append)))

(setq disabled-command-function nil)

(setq frame-inhibit-implied-resize t)
(setq initial-major-mode 'fundamental-mode)

;; TODO Dired config and using dired
;; TODO Registers?
;; TODO Bookmarks?


